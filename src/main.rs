// ===== :53
// ===== :864 - (Imports)
use std::{
    env,
    fs::OpenOptions,
    io::{
        self,
        BufRead,
        Write
    }
};

// ===== :886 - (Constants)
const BLOCK_MARKER:         &str = r"\\";
const LABELED_BLOCK_MARKER: &str = r"\\@";
const CONT_BLOCK_MARKER:    &str = r"\\-";
const FORWARD_REF_MARKER:   &str = r"@>";
const BACKWARD_REF_MARKER:  &str = r"<@";

// ===== :69 - The till parser + compiler
// ===== :90 - Data structures
#[derive(Clone)]
struct Block {
    tag: BlockTag,
    line: usize,
    text: Vec<String>
}

#[derive(Clone, PartialEq)]
enum BlockTag {
    Doc,
    Code,
    LabeledCode(String),
    ContCode
}

impl Block {
    fn new_docs(line: usize) -> Block {
        Block {
            tag: BlockTag::Doc,
            line,
            text: Vec::new()
        }
    }

    fn new_code(line: usize) -> Block {
        Block {
            tag: BlockTag::Code,
            line,
            text: Vec::new()
        }
    }

    fn new_labeled_code(label: String, line: usize) -> Block {
        Block {
            tag: BlockTag::LabeledCode(label),
            line,
            text: Vec::new()
        }
    }

    fn new_cont_code(line: usize) -> Block {
        Block {
            tag: BlockTag::ContCode,
            line,
            text: Vec::new()
        }
    }

    // ===== :898 - (fn Block::trim)
    fn trim(&mut self) {
        let mut start = 0;
        for line in self.text.iter() {
            let is_blank = line.trim().len() == 0;
            if !is_blank { break; }
            start += 1;
        }
    
        let mut end = self.text.len();
        for line in self.text.iter().rev() { 
            let is_blank = line.trim().len() == 0;
            if !is_blank { break; }
            end -= 1;
        }
    
        self.text = self.text
            .clone()
            .into_iter()
            .skip(start)
            .take(end - start)
            .collect();
        self.line += start;
    }
}

// ===== :153
enum CodeTree {
    Leaf(String),
    Node {
        tag: NodeTag,
        whitespace: String,
        line: usize,
        children: Vec<CodeTree>
    }
}

#[derive(Clone)]
enum NodeTag {
    Root,
    TopLevel,
    Labeled(String),
    ContTopLevel,
    ContLabeled(String)
}

impl CodeTree {
    fn new_leaf(line: String) -> Self {
        CodeTree::Leaf(line)
    }

    fn new_node(tag: NodeTag, line: usize) -> Self {
        CodeTree::Node {
            tag,
            whitespace: "".to_string(),
            line,
            children: Vec::new()
        }
    }

    // Push a new child into a Node
    fn push(&mut self, tree: CodeTree) {
        if let CodeTree::Node { ref mut children, .. } = self {
            children.push(tree);
            return;
        }

        // Should never get here
        panic!("Tried to push child into a leaf node?");
    }
}

// ===== :928 - (impl NodeTag)
impl NodeTag {
    fn continued(&self) -> Self {
        match self {
            NodeTag::TopLevel     => NodeTag::ContTopLevel,
            NodeTag::ContTopLevel => NodeTag::ContTopLevel,

            NodeTag::Labeled(label)     => NodeTag::ContLabeled(label.clone()),
            NodeTag::ContLabeled(label) => NodeTag::ContLabeled(label.clone()),

            _ => panic!("Tried to continue the root?")
        }
    }

    fn is_top_level(&self) -> bool {
        match self {
            NodeTag::TopLevel     => true,
            NodeTag::ContTopLevel => true,
            _ => false
        }
    }

    fn is_continued(&self) -> bool {
        match self {
            NodeTag::ContTopLevel   => true,
            NodeTag::ContLabeled(_) => true,
            _ => false
        }
    }
}

// ===== :207
#[derive(Debug)]
enum ParserError {
    ReusedRef(String, usize),  // (label, line number)
    RefNotFound(String, usize) // (label, line number)
}

// ===== :227 - The parser
fn parse(
    args: &Args,
    input: &mut dyn io::Read
) -> Result<(Vec<Block>, CodeTree), Error> {
    let mut input = io::BufReader::new(input);

    // ===== :249 - ...fn parse
    let mut block_list = Vec::new();
    let mut curr_block = Block::new_docs(1);
    let mut line_idx = 1;
    
    loop {
        let mut line = String::new();
        let n_bytes = input.read_line(&mut line)?;
        // End-of-file
        if n_bytes == 0 { break; }
    
        if line.starts_with(LABELED_BLOCK_MARKER) {
            // Parse line for label
            let label = line
                .strip_prefix(LABELED_BLOCK_MARKER)
                .unwrap()
                .trim()
                .to_string();
            let next_block = Block::new_labeled_code(label, line_idx+1);
            
            block_list.push(curr_block);
            curr_block = next_block;
        }
        else if line.starts_with(CONT_BLOCK_MARKER) {
            let next_block = Block::new_cont_code(line_idx+1);
    
            block_list.push(curr_block);
            curr_block = next_block;
        }
        else if line.starts_with(BLOCK_MARKER) {
            // Switch docs/code
            let next_block = match curr_block.tag {
                BlockTag::Doc => Block::new_code(line_idx+1),
                _             => Block::new_docs(line_idx+1),
            };
    
            block_list.push(curr_block);
            curr_block = next_block;
        }
        else {
            curr_block.text.push(line);
        }
    
        line_idx += 1;
    }
    
    // Last block
    block_list.push(curr_block);
    
    // Trim whitespace if needed
    if !args.preserve {
        for block in block_list.iter_mut() {
            if block.tag != BlockTag::Doc {
                block.trim();
            }
        }
    }

    // ===== :317
    let code = block_list
        .iter()
        .filter(|block| block.tag != BlockTag::Doc)
        .map(|block| block.clone())
        .collect::<Vec<Block>>();
    
    let docs = block_list;

    // ===== :337
    let mut used = vec![false; code.len()];
    let mut tree = CodeTree::new_node(NodeTag::Root, 0);
    
    for idx in 0..code.len() {
        if let BlockTag::LabeledCode(_) = code[idx].tag { continue; }
        if let BlockTag::ContCode = code[idx].tag { continue; }
    
        let subtree = resolve(idx, NodeTag::TopLevel, &code, &mut used)?;
        tree.push(subtree);
    }
    
    return Ok((docs, tree));
}

// ===== :360 - fn resolve
fn resolve(
    idx: usize,
    tag: NodeTag,
    code: &Vec<Block>,
    used: &mut Vec<bool>
) -> Result<CodeTree, ParserError> {
    used[idx] = true;

    let Block { text, line: line_bias, .. } = &code[idx];

    let mut tree = CodeTree::new_node(tag.clone(), *line_bias);
    for (line_idx, line) in text.iter().enumerate() {
        let line_idx = line_bias + line_idx;
        // ===== :388 - Process each line
        let mut ref_type = None;
        let content = line.trim_start();
        if content.starts_with(FORWARD_REF_MARKER) {
            ref_type = Some(true);
        }
        else if content.starts_with(BACKWARD_REF_MARKER) {
            ref_type = Some(false);
        }
        
        if let Some(is_forward) = ref_type {
            // Remove marker, trim to get label
            let marker =
                if is_forward { FORWARD_REF_MARKER  }
                else          { BACKWARD_REF_MARKER };
            let label = content
                .strip_prefix(marker)
                .unwrap()
                .trim()
                .to_string();
        
            // ===== :426 - Process the ref
            let mut ref_idx = None;
            // Rust trick to dynamically dispatch the iterator function for different types
            let range: Box<dyn Iterator<Item = usize>> =
                if !is_forward { Box::new((0..=(idx-1)).rev()) }
                else           { Box::new((idx+1)..code.len()) };
            for idx_ in range {
                if let BlockTag::LabeledCode(label_) = &code[idx_].tag {
                    if label == *label_ {
                        ref_idx = Some(idx_);
                        break;
                    }
                }
            }
            
            if ref_idx.is_none() { 
                return Err(ParserError::RefNotFound(label, line_idx));
            }
            let ref_idx = ref_idx.unwrap();
            
            if used[ref_idx] {
                return Err(ParserError::ReusedRef(label, line_idx));
            }
            
            let mut subtree = resolve(ref_idx, NodeTag::Labeled(label.clone()), code, used)?;
            
            let whitespace_len = line.len() - content.len();
            // Have to destructure this despite knowing what variant it is, thanks Rust
            if let CodeTree::Node { ref mut whitespace, .. } = subtree {
                *whitespace = line
                    .get(0..whitespace_len)
                    .unwrap()
                    .to_string();
            }
            
            tree.push(subtree);
        }
        else {
            let leaf = CodeTree::new_leaf(line.clone());
            tree.push(leaf);
        }
    }

    // ===== :469 - Check for a cont. block
    if idx+1 < code.len() {
        if let BlockTag::ContCode = code[idx+1].tag {
            let subtree = resolve(idx+1, tag.continued(), code, used)?;
            tree.push(subtree);
        }
    }

    return Ok(tree);
}

// ===== :487 - The compiler
fn compile(
    args: &Args,
    docs: Vec<Block>,
    code: CodeTree,
    docs_output: &mut dyn io::Write,
    code_output: &mut dyn io::Write
) -> Result<(), Error> {
    let mut docs_output = io::BufWriter::new(docs_output);
    let mut code_output = io::BufWriter::new(code_output);

    write_docs(&docs, &mut docs_output, args)?;
    write_code(&code, &mut code_output, args)?;

    return Ok(());
}

// ===== :511
fn write_docs<W: Write>(
    docs: &Vec<Block>,
    output: &mut io::BufWriter<W>,
    _args: &Args
) -> Result<(), io::Error> {
    for Block { tag, text, .. } in docs.iter() {
        match tag {
            BlockTag::Doc => {
                for line in text.iter() {
                    write!(output, "{}", line)?;
                }
            },
            BlockTag::Code => {
                writeln!(output, "```")?;
                for line in text.iter() {
                    write!(output, "{}", line)?;
                }
                writeln!(output, "```")?;
            },
            BlockTag::LabeledCode(label) => {
                writeln!(output, "**{}**", label)?;
                writeln!(output, "```")?;
                for line in text.iter() {
                    write!(output, "{}", line)?;
                }
                writeln!(output, "```")?;
            },
            BlockTag::ContCode => {
                writeln!(output, "```")?;
                for line in text.iter() {
                    write!(output, "{}", line)?;
                }
                writeln!(output, "```")?;
            }
        }
    }

    return Ok(());
}

// ===== :558
fn write_code<W: Write>(
    code: &CodeTree,
    output: &mut io::BufWriter<W>,
    args: &Args
) -> Result<(), io::Error> {
    write_code_tree(&"".to_string(), &code, output, args)
}

// ===== :568
fn write_code_tree<W: Write>(
    whitespace: &String,
    code: &CodeTree,
    output: &mut io::BufWriter<W>,
    args: &Args
) -> Result<(), io::Error> {
    match code {
        CodeTree::Leaf(line) => {
            write!(output, "{}{}", whitespace, line)?;
        },
        CodeTree::Node { tag, whitespace: whitespace_, line, children } => {
            // Write a blank line before continued blocks
            if !args.preserve && tag.is_continued() {
                writeln!(output)?;
            }

            let mut whitespace = whitespace.clone();
            whitespace.push_str(&whitespace_);

            // Section header comment
            match tag {
                NodeTag::TopLevel => {
                    writeln!(output, r"{}{} ===== :{}", whitespace, args.comment, line)?;
                },
                NodeTag::ContTopLevel => {
                    writeln!(output, r"{}{} ===== :{}", whitespace, args.comment, line)?;
                },
                NodeTag::Labeled(label) => {
                    writeln!(output, r"{}{} ===== :{} - {}", whitespace, args.comment, line, label)?;
                },
                NodeTag::ContLabeled(_) => {
                    writeln!(output, r"{}{} ===== :{}", whitespace, args.comment, line)?;
                },
                _ => {}
            }

            // Write the content of the code tree
            let mut has_cont = false;
            for child in children.iter() {
                write_code_tree(&whitespace, &child, output, args)?;

                if let CodeTree::Node { tag, .. } = child {
                    if tag.is_continued() {
                        has_cont = true;
                    }
                }
            }

            // Write a blank line at the end of a top-level block with no continued block after
            if !args.preserve && !has_cont && tag.is_top_level() {
                writeln!(output)?;
            }
        }
    }

    return Ok(());
}

// ===== :632 - The till app
// ===== :644 - Data structures
struct Args {
    docs_only: bool,
    code_only: bool,
    stdin: bool,
    stdout: bool,
    preserve: bool,
    comment: String
}

impl Args {
    fn new() -> Self {
        Args {
            docs_only: false,
            code_only: false,
            stdin: false,
            stdout: false,
            preserve: false,
            // TODO make this configurable somehow, maybe read input file name?
            comment: "//".to_string()
        }
    }
}

// ===== :673
#[derive(Debug)]
enum ArgError {
    DocsAndCodeOnly,
    DocsAndCodeToStdout,
    TooManyArgs,
    NotEnoughArgs
}

// ===== :690 - The main function
fn main() -> Result<(), Error> {
    let mut args = Args::new();
    let mut pos_args = Vec::new();

    for arg in env::args().skip(1) {
        if arg.starts_with("-") {
            for ch in arg.chars().skip(1) {
                let flag = match ch {
                    'd' => &mut args.docs_only,
                    'c' => &mut args.code_only,
                    'i' => &mut args.stdin,
                    'o' => &mut args.stdout,
                    'p' => &mut args.preserve,
                    _ => { todo!(); }
                };
                *flag = true;
            }
        }
        else {
            pos_args.push(arg.clone());
        }
    }

// ===== :723
    if args.docs_only && args.code_only {
        return Err(ArgError::DocsAndCodeOnly.into());
    }
    if !args.docs_only && !args.code_only && args.stdout {
        return Err(ArgError::DocsAndCodeToStdout.into());
    }

    let mut pos_args = pos_args.into_iter();
    let mut next_pos_arg = move || {
        pos_args
            .next()
            .ok_or::<Error>(ArgError::NotEnoughArgs.into())
    };

    let input_path =
        if args.stdin { None                  }
        else          { Some(next_pos_arg()?) };

    let docs_output_path =
        if args.code_only   { None                  }
        else if args.stdout { None                  }
        else                { Some(next_pos_arg()?) };
        
    let code_output_path =
        if args.docs_only   { None                  }
        else if args.stdout { None                  }
        else                { Some(next_pos_arg()?) };

    if let Ok(_) = next_pos_arg() {
        return Err(ArgError::TooManyArgs.into());
    }

// ===== :761
    let mut input: Box<dyn io::Read>;
    if args.stdin {
        input = Box::new(io::stdin());
    }
    else {
        let file = OpenOptions::new()
            .read(true)
            .open(input_path.unwrap())?;
        input = Box::new(file);
    }

    let (docs, code) = parse(&args, &mut input)?;

// ===== :780
    let mut docs_output: Box<dyn io::Write>;
    if args.code_only {
        // Very handy Rust std util, essentially a programmatic /dev/null
        docs_output = Box::new(io::sink());
    }
    else if args.stdout {
        docs_output = Box::new(io::stdout());
    }
    else {
        let file = OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(docs_output_path.unwrap())?;
        docs_output = Box::new(file);
    }

    let mut code_output: Box<dyn io::Write>;
    if args.docs_only {
        code_output = Box::new(io::sink());
    }
    else if args.stdout {
        code_output = Box::new(io::stdout());
    }
    else {
        let file = OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(code_output_path.unwrap())?;
        code_output = Box::new(file);
    }

    compile(&args, docs, code, &mut docs_output, &mut code_output)?;

    return Ok(());
}

// ===== :827 - Error handling
// TODO add pretty-printing instead of using derived Debug
#[derive(Debug)]
enum Error {
    Arg(ArgError),
    Parser(ParserError),
    Other(String)
}

impl From<ArgError> for Error {
    fn from(error: ArgError) -> Self {
        Error::Arg(error)
    }
}

impl From<ParserError> for Error {
    fn from(error: ParserError) -> Self {
        Error::Parser(error)
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::Other(format!("{}", error))
    }
}

