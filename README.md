# Till: A minimal tool for writing literate programs

Till is a minimal tool to help you write literate programs.
When you run `till` on a .till source file, it weaves all of the documentation + code into a formatted Markdown document and tangles the code into a source code file.

**Usage**
```
till [-dciop] [input file] [docs output file] [code output file]
```

Flag | Meaning
--- | ---
`-d` | Generate docs only
`-c` | Generate code only
`-i` | Read input from stdin
`-o` | Write output to stdout (requires one of `-d`, `-c`)
`-p` | Preserve source code formatting

This README and the source code (src/main.rs) were generated with `till`.
If you want to see it in action, you can re-generate README.md/main.rs for yourself:
```
cargo run src/main.rs.till README.md src/main.rs
```

## Overview

Till takes a very simple approach to literate programming, allowing your source files to remain clean and easily editable.

Blocks of text in `till` always belong to either the documentation or code.
When `till` generates the documentation file, doc blocks and code blocks just get pasted in the order they appear (with the code blocks formatted).
However, when `till` generates the source code file, code blocks may be reordered and merged together by using labels and references.

A label is just a name for a block of code.
When you create a regular, "top-level" block of code, it gets pasted straight into the source file.
But when you create a labeled block of code, it will get pasted wherever it gets referenced.
A reference happens _within_ a block of code, and can refer to a labeled block before or after its own block.
Labels can be reused as names, but each labeled block may only be used once - this is more restrictive than say, a macro or function.

The other way that blocks can be put together is with continued blocks of code, which just attach themselves to the previous block.
This is especially useful for cleanly handling long labeled blocks of code.

References let you turn complex pieces of code into more manageable parts with ad-hoc names.
Continued blocks let you split up long sections of code with in-depth explanations.
Together, they let you turn a mess of code into something that can be read top-to-bottom.

# Till

To start, here's the layout of the completed source file, to let you know what we're working towards.
_Convention note: a label in (parentheses) means that it appears in the appendix._

```rs
@> (Imports)

@> (Constants)

@> The till parser + compiler

@> The till app

@> (Error handling)
```

## Parsing and compiling .till files

**The till parser + compiler**
```rs
@> Data structures

@> The parser

@> The compiler
```

### Data structures

We'll start by defining the data structures that the parser will be using later.

The parser works in 2 passes: in the first pass, the .till file is read line-by-line to split it into blocks of documentation and code.
In the second pass, references in code blocks are resolved to find the block that they point to, building a tree of code.
This tree can then be flattened into source code.

`Block`s are very simple - each consists of a list of lines, along with information about whether they're a doc/code/labeled code/continued code block.
We implement some constructors, and a `trim` function that will remove leading/trailing whitespace lines.

**Data structures**
```rs
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

    @> (fn Block::trim)
}
```

And `CodeTree`s are a typical tree structure, built from a union of nodes and leaves (lines of code).
Because `CodeTree`s are the structure that gets written directly to the code output file, some extra info is needed for formatting `Node`s nicely:
- Whether this block is continued or not, and whether it belongs to a top-level or labeled block
- The `whitespace` that precedes it
- The original line that this block starts on.

The `NodeTag` enum has a couple of convenience functions so that we don't need to constantly `match` against its variants.

```rs
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

@> (impl NodeTag)
```

Lastly, an error type.
There aren't many different reasons for the parser to fail, due to its simplicity.

```rs
#[derive(Debug)]
enum ParserError {
    ReusedRef(String, usize),                  // (label, line number)
    RefNotFound(String, usize),                // (label, line number)
    UnusedBlocks(Vec<(Option<String>, usize)>) // [(label, line number)]
}
```

### The parser

Parsing (turning input into structured data) and compiling (turning structured data into output) are two separate steps, which get composed by `main`.
We may have to read/write using different types of sources (stdin/stdout/file), so we'll keep things general and make each type `dyn Read/Write`, and then immediately turn them into buffered readers/writers.

TODO explain errors

The parser also relies on a helper function (`resolve`), which is explained later.

**The parser**
```rs
fn parse(
    args: &Args,
    input: &mut dyn io::Read
) -> Result<(Vec<Block>, CodeTree), Error> {
    let mut input = io::BufReader::new(input);

    @> ...fn parse
}

@> fn resolve
```

Now comes stage 1 of the parse: turning the .till source into a sequence of blocks.
There are only a few rules to follow:
1. The first block is documentation
2. Encountering a line with just a `BLOCK_MARKER` (`\\`) token switches between documentation and top-level code
3. Encountering a line starting with the `LABELED_BLOCK_MARKER` (`\\@`) starts a new labeled block of code
4. Encountering a line starting with the `CONT_BLOCK_MARKER` (`\\-`) starts a block of code that will be attached to the previous block of code.

**...fn parse**
```rs
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
```

At this point, we've built the list of blocks and tagged them with the appropriate type+label.
We can now move on to stage 2, where we transform the list of blocks into the right structure for outputting docs and code to files.

First, split the list into two: one for docs and one for code.
The code needs to only hold code-tagged blocks.
The docs comprises both code and docs, so we can just rename `block_list` to `docs` and call it done.

```rs
let code = block_list
    .iter()
    .filter(|block| block.tag != BlockTag::Doc)
    .map(|block| block.clone())
    .collect::<Vec<Block>>();

let docs = block_list;
```

That's all the processing needed for documentation.
Now, the only thing that's left to do is transform the list of code blocks into a tree.

The majority of the complexity here is handled by the `resolve` function, which takes a code block and resolves all of its references.
We just have to feed it the top-level blocks in the order presented in the document.

At the end, we return from `parse`.

```rs
let mut used = vec![false; code.len()];
let mut tree = CodeTree::new_node(NodeTag::Root, 0);

for idx in 0..code.len() {
    if let BlockTag::LabeledCode(_) = code[idx].tag { continue; }
    if let BlockTag::ContCode = code[idx].tag { continue; }

    let subtree = resolve(idx, NodeTag::TopLevel, &code, &mut used)?;
    tree.push(subtree);
}

// Find unused blocks, if any
let mut unused = Vec::new();
for idx in 0..code.len() {
    if !used[idx] {
        match &code[idx].tag {
            BlockTag::LabeledCode(label) => {
                unused.push((Some(label.clone()), code[idx].line));
            },
            BlockTag::ContCode => {
                unused.push((None, code[idx].line));
            },
            _ => todo!()
        }
    }
}

if unused.len() > 0 {
    return Err(ParserError::UnusedBlocks(unused).into());
}

return Ok((docs, tree));
```

`used` is a vector that tracks whether each block has been used in a reference or not, so that we can track reference errors without modifying `code`.
`Node`s are constructed with a tag that identifies what type of block underlies the node, so this is also passed to `resolve`.

There are two tasks needed to resolve the code tree: process each line for references, and then check for a continued code block ahead.
At the end, we return the tree that we've built.

**fn resolve**
```rs
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
        @> Process each line
    }

    @> Check for a cont. block

    return Ok(tree);
}
```

Each line may be a regular line of code, or a reference.
If we see that the trimmed line starts with a ref marker, then we proceed with ref processing - otherwise, we can simply add the line as a leaf in the tree.

**Process each line**
```rs
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

    @> Process the ref
}
else {
    let leaf = CodeTree::new_leaf(line.clone());
    tree.push(leaf);
}
```

Now we iterate forward or backward in the code blocks to find a matching label.

After finding the `ref_idx`, we can recursively call `resolve` and start the process all over again, making sure to bubble up any errors.

The absolute last thing we need to do is get the whitespace from this block and stick it in the new node we've created.
Then we can add the node to the tree we're building.

**Process the ref**
```rs
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
```

Before we return from `resolve`, there's one more ref we need to check for - there might be a continued code block after this one, which we'll need to attach as well.
We don't need to check whether this block has been used - it comes after our current block and can't be referenced by label.

**Check for a cont. block**
```rs
if idx+1 < code.len() {
    if let BlockTag::ContCode = code[idx+1].tag {
        let subtree = resolve(idx+1, tag.continued(), code, used)?;
        tree.push(subtree);
    }
}
```

After this, we're done parsing!
Both the documentation and the code are in a good state to start writing them to the output files, so we can introduce the functions that handle writing the output to files.

### The compiler

Brace yourself, here's the entire `compile` function:

**The compiler**
```rs
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
```

Obviously, the complexity is hidden behind other functions again.

`write_docs` is simple, and just writes each line as-is with a little extra formatting for code blocks.

```rs
fn write_docs<W: Write>(
    docs: &Vec<Block>,
    output: &mut io::BufWriter<W>,
    args: &Args
) -> Result<(), io::Error> {
    let syntax_lang =
        if let Some(lang) = &args.lang { lang }
        else                           { ""   };

    for Block { tag, text, .. } in docs.iter() {
        match tag {
            BlockTag::Doc => {
                for line in text.iter() {
                    write!(output, "{}", line)?;
                }
            },
            BlockTag::Code => {
                writeln!(output, "```{}", syntax_lang)?;
                for line in text.iter() {
                    write!(output, "{}", line)?;
                }
                writeln!(output, "```")?;
            },
            BlockTag::LabeledCode(label) => {
                writeln!(output, "**{}**", label)?;
                writeln!(output, "```{}", syntax_lang)?;
                for line in text.iter() {
                    write!(output, "{}", line)?;
                }
                writeln!(output, "```")?;
            },
            BlockTag::ContCode => {
                writeln!(output, "```{}", syntax_lang)?;
                for line in text.iter() {
                    write!(output, "{}", line)?;
                }
                writeln!(output, "```")?;
            }
        }
    }

    return Ok(());
}
```

The `write_code` function is more complex but doesn't directly handle most of the work, instead acting as a proxy for the `write_code_tree` function.
Each block of code may itself be a deeper reference - all the whitespace of each block is collected in the `whitespace` parameter and passed down through recursive calls.
In order to print the tree in the right order, we simply traverse it from top to bottom.



```rs
fn write_code<W: Write>(
    code: &CodeTree,
    output: &mut io::BufWriter<W>,
    args: &Args
) -> Result<(), io::Error> {
    write_code_tree(&"".to_string(), &code, output, args)
}
```
```rs
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
            if let Some(comment) = &args.comment {
                match tag {
                    NodeTag::TopLevel => {
                        writeln!(output, r"{}{} ===== :{}", whitespace, comment, line)?;
                    },
                    NodeTag::ContTopLevel => {
                        writeln!(output, r"{}{} ===== :{}", whitespace, comment, line)?;
                    },
                    NodeTag::Labeled(label) => {
                        writeln!(output, r"{}{} ===== :{} - {}", whitespace, comment, line, label)?;
                    },
                    NodeTag::ContLabeled(_) => {
                        writeln!(output, r"{}{} ===== :{}", whitespace, comment, line)?;
                    },
                    _ => {}
                }
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
```

## Creating the `till` CLI

**The till app**
```rs
@> Data structures

@> The main function
```

### Data structures

First, a simple data structure to hold the program args.

**Data structures**
```rs
struct Args {
    docs_only: bool,
    code_only: bool,
    stdin: bool,
    stdout: bool,
    preserve: bool,

    lang: Option<String>,
    comment: Option<String>
}

impl Args {
    fn new() -> Self {
        Args {
            docs_only: false,
            code_only: false,
            stdin: false,
            stdout: false,
            preserve: false,
            lang: None,
            comment: None
        }
    }
}
```

And a data structure to handle arg errors.

```rs
#[derive(Debug)]
enum ArgError {
    UnknownFlag(char), // (flag)
    DocsAndCodeOnly,
    DocsAndCodeToStdout,
    TooManyArgs,
    NotEnoughArgs
}
```

### The main function

Finally, we're at the entrypoint of the app.
The main function itself wraps around a secondary function, `run`.
This allows us to more cleanly report errors and return.
The first thing the app needs to do is parse the command-line args to extract the flags and positional arguments.

**The main function**
```rs
fn main() -> ExitCode {
    match run() {
        Err(error) => {
            eprintln!("Error: {}", error);
            ExitCode::FAILURE
        },
        _ => ExitCode::SUCCESS
    }
}

fn run() -> Result<(), Error> {
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
                    c => {
                        return Err(ArgError::UnknownFlag(c).into());
                    }
                };
                *flag = true;
            }
        }
        else {
            pos_args.push(arg.clone());
        }
    }
```

Then, we start validating the arguments.
There are a few rules to uphold, based on the flags that are set:
- Only one of -d and -c may be set
- If we're producing both docs and code, then they must get written to files, not stdout
- We need exactly 1 positional arg for each file that needs to be read from/written to.

```rs
    if args.docs_only && args.code_only {
        return Err(ArgError::DocsAndCodeOnly.into());
    }
    if !args.docs_only && !args.code_only && args.stdout {
        return Err(ArgError::DocsAndCodeToStdout.into());
    }

    let mut pos_args = pos_args.into_iter();
    let mut next_pos_arg = || {
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
```

Here we can also check for a language extension.
By convention, if your file extension is ".lang.till", then `till` will read lang as the source language name and provide additional formatting - section comments in the source code and Markdown-enabled syntax highlighting in the documentation.
Comment styles are hardcoded as a string -> string map, from lang extension to comment marker token.

```rs
    if let Some(input_path_text) = &input_path {
        // Reverse-split input_path to get something like [till, <lang>, filename, ..]
        let exts: Vec<&str> = input_path_text
            .rsplit('.')
            .collect();
        
        if exts.len() >= 3 && exts[0] == "till" {
            let lang = exts[1];

            args.lang = Some(lang.to_string());
            args.comment = COMMENT_STYLES
                .get(lang)
                .map(|comment| comment.to_string()); // Maps the interior of the Option
        }
    }
```

With the args parsed and validated, we can start by parsing:

```rs
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
```

...and if that went well, then we can pass the result to the compiler:

```rs
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
```

## Appendix

These are bits of code that weren't important or complex enough to warrant discussion when they were first used.

Imports!
Usually these come first, but since we're mostly just importing some std libraries, I wanted to keep them from turning into extra visual clutter.

**(Imports)**
```rs
use lazy_static::lazy_static; // For the language-comment map
use std::{
    collections::HashMap,
    env,
    error,
    fmt,
    fs::OpenOptions,
    io::{
        self,
        BufRead,
        Write
    },
    process::ExitCode
};
```

These constants define what the parser reconizes.

Five different tokens need to be recognized by the parser:
- One token to denote a switch from documentation to code
- One token to denote a labeled block of code
- One token to denote a continued block of code
- Two tokens for forward/backward references.

We also use a lazy_static block so that we can treat the COMMENT_STYLES HashMap as a const.

**(Constants)**
```rs
const BLOCK_MARKER:         &str = r"\\";
const LABELED_BLOCK_MARKER: &str = r"\\@";
const CONT_BLOCK_MARKER:    &str = r"\\-";
const FORWARD_REF_MARKER:   &str = r"@>";
const BACKWARD_REF_MARKER:  &str = r"<@";

lazy_static! {
    static ref COMMENT_STYLES: HashMap<&'static str, &'static str> = {
        HashMap::from_iter(
            [
                ("rs",   "//"),
                ("c",    "//"),
                ("cpp",  "//"),
                ("java", "//"),
                ("py",   "#" ),
                ("js",   "//"),
                ("ts",   "//"),
                ("hs",   "--")
            ].into_iter()
        )
    };
}
```

A single `Error` type is used - a dynamic reference to a `std::error::Error`.
This has the advantage of allowing us to define multiple error types while easily handling them all separately.
Also, `io::Error` derives this type, so we can natively return IO errors without any extra handling.

**(Error handling)**
```rs
type Error = Box<dyn error::Error>;

impl error::Error for ArgError {}

impl fmt::Display for ArgError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            ArgError::UnknownFlag(c) => write!(f,
                "Unknown flag -{}",
                c
            ),
            ArgError::DocsAndCodeOnly => write!(f,
                concat!(
                    "Can only use one of -d, -c.\n",
                    "If you want to generate both docs and code, use file outputs:\n",
                    "    till <input.x.till> <docs-output.md> <code-output.x>"
                ),
            ),
            ArgError::DocsAndCodeToStdout => write!(f,
                concat!(
                    "Cannot send both docs and code to stdout.\n",
                    "Try either running with -d/-c, or using file outputs:",
                    "    till <input.x.till> <docs-output.md> <code-output.x>"
                )
            ),
            ArgError::TooManyArgs => write!(f,
                "Too many args",
            ),
            ArgError::NotEnoughArgs => write!(f,
                "Not enough arguments",
            )
        }
    }
}

impl error::Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            ParserError::ReusedRef(label, line) => write!(f,
                concat!(
                    "Reused reference \"{}\" (line {}).\n",
                    "Label names may be reused, but each labeled block must be referenced exactly once",
                ),
                label,
                line
            ),
            ParserError::RefNotFound(label, line) => write!(f,
                "Reference \"{}\" (line {}) not found",
                label,
                line
            ),
            ParserError::UnusedBlocks(occurrences) => {
                write!(f, "{} unused code block(s):", occurrences.len())?;
                for (label, line) in occurrences.iter() {
                    if let Some(label) = label {
                        write!(f, "\n    Label \"{}\" (line {})", label, line-1)?;
                    }
                    else {
                        write!(f, "\n    Continuation block (line {})", line-1)?;
                    }
                }
                Ok(())
            }
        }
    }
}
```

This is the function that strips blocks of their leading/trailing whitespace.

**(fn Block::trim)**
```rs
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

    // Occurs if a block is all whitespace
    if start > end {
        start = 0;
    }

    self.text = self.text
        .clone()
        .into_iter()
        .skip(start)
        .take(end - start)
        .collect();
    self.line += start;
}
```

This is the little bit of boilerplate that helps us avoid a lot of destructuring when dealing with `NodeTag`s.

**(impl NodeTag)**
```rs
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
```
