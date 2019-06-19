
#![allow(dead_code)]

use std::cell::Cell;
use std::rc::Rc;
use std::fmt::Write;

#[derive(Clone,Copy,PartialEq)]
pub enum Symbol {
    Terminal, Item, Add, Sub, Mul, Div, Neg, Circumflex, Amp, Vert,
    Assignment, Eq, Ne, Not, Qm, PathSep,
    Dot, Comma, Colon, Semicolon,
    LeftParen, RightParen,
    LeftBracket, RightBracket,
    LeftBrace, RightBrace,

    /* Keywords */
    And, Break, Continue,
    Else, Enum, False, Fn, For, If, In, Let, Loop,
    Match, Mod, Mut,
    Or, Pub, Return, Struct, Trait, True, Type,
    Use, Where, While
}

impl Symbol {
    fn as_str(&self) -> &'static str {
        match self {
            Symbol::Terminal => "Terminal",
            Symbol::Item => "Item",
            Symbol::Add => "+",
            Symbol::Sub => "-",
            Symbol::Mul => "*",
            Symbol::Div => "/",
            Symbol::Neg => "-",
            Symbol::Circumflex => "^",
            Symbol::Amp => "&",
            Symbol::Vert => "|",
            Symbol::Assignment => "=",
            Symbol::Eq => "==",
            Symbol::Ne => "!=",
            Symbol::Not => "not",
            Symbol::Qm => "?",
            Symbol::Dot => ".",
            Symbol::Comma => ",",
            Symbol::Colon => ":",
            Symbol::Semicolon => ";",
            Symbol::PathSep => "::",
            Symbol::LeftParen => "(",
            Symbol::RightParen => ")",
            Symbol::LeftBracket => "[",
            Symbol::RightBracket => "]",
            Symbol::LeftBrace => "{",
            Symbol::RightBrace => "}",
            Symbol::And => "and",
            Symbol::Break => "break",
            Symbol::Continue => "continue",
            Symbol::Else => "else",
            Symbol::Enum => "enum",
            Symbol::False => "false",
            Symbol::Fn => "fn",
            Symbol::For => "for",
            Symbol::If => "if",
            Symbol::In => "in",
            Symbol::Let => "let",
            Symbol::Loop => "loop",
            Symbol::Match => "match",
            Symbol::Mod => "mod",
            Symbol::Mut => "mut",
            Symbol::Or => "or",
            Symbol::Pub => "pub",
            Symbol::Return => "return",
            Symbol::Struct => "struct",
            Symbol::Trait => "trait",
            Symbol::True => "true",
            Symbol::Type => "type",
            Symbol::Use => "use",
            Symbol::Where => "where",
            Symbol::While => "while"
        }
    }
}

pub enum Item {
    None, Int(u64), Id(String)
}

#[allow(dead_code)]
pub struct Token {
    line: u32,
    col: u32,
    symbol: Symbol,
    item: Item,
}
impl Token {
    fn symbol(line: u32, col: u32, symbol: Symbol) -> Token {
        Token{line, col, symbol, item: Item::None}
    }
}

#[allow(dead_code)]
pub struct SyntaxErrorStruct {
    pub line: u32,
    pub col: u32,
    pub text: String
}

type SyntaxError = Box<SyntaxErrorStruct>;

fn syntax_error(line: u32, col: u32, text: String) -> SyntaxError {
    Box::new(SyntaxErrorStruct{line,col,text})
}

type KeywordsElement = (&'static str, &'static Symbol);

static KEYWORDS: &'static [KeywordsElement] = &[
    ("and",     &Symbol::And),
    ("break",   &Symbol::Break),
    ("continue",&Symbol::Continue),
    ("else",    &Symbol::Else),
    ("enum",    &Symbol::Enum),
    ("false",   &Symbol::False),
    ("fn",      &Symbol::Fn),
    ("for",     &Symbol::For),
    ("if",      &Symbol::If),
    ("in",      &Symbol::In),
    ("let",     &Symbol::Let),
    ("loop",    &Symbol::Loop),
    ("match",   &Symbol::Match),
    ("mod",     &Symbol::Mod),
    ("mut",     &Symbol::Mut),
    ("not",     &Symbol::Not),
    ("or",      &Symbol::Or),
    ("pub",     &Symbol::Pub),
    ("return",  &Symbol::Return),
    ("struct",  &Symbol::Struct),
    ("trait",   &Symbol::Trait),
    ("true",    &Symbol::True),
    ("type",    &Symbol::Type),
    ("use",     &Symbol::Use),
    ("where",   &Symbol::Where),
    ("while",   &Symbol::While)
];

fn is_keyword(id: &String) -> Option<&'static KeywordsElement> {
    let n: usize = KEYWORDS.len();
    for i in 0..n {
        if KEYWORDS[i].0 == id  {return Some(&KEYWORDS[i]);}
    }
    return None;
}

#[allow(dead_code)]
fn item_to_string(buffer: &mut String, item: &Item) {
    match item {
        Item::Id(id) => buffer.push_str(&id),
        Item::Int(x) => buffer.push_str(&format!("{}",x)),
        Item::None => buffer.push_str("Item::None")
    }
}

fn token_to_string(buffer: &mut String, t: &Token) {
    if let Symbol::Item = t.symbol {
        item_to_string(buffer,&t.item);
    }else{
        buffer.push_str(t.symbol.as_str());
    }
}

#[allow(dead_code)]
fn vec_token_to_string(v: &Vec<Token>) -> String {
    let mut buffer = String::new();
    let mut first = true;
    buffer.push_str("[");
    for x in v {
        if first {first = false;}
        else {buffer.push_str(", ");}
        token_to_string(&mut buffer,x);
    }
    buffer.push_str("]");
    return buffer;
}

#[allow(dead_code)]
pub fn print_vec_token(v: &Vec<Token>) {
    println!("{}",vec_token_to_string(v));
}

pub fn scan(input: &str) -> Result<Vec<Token>,SyntaxError> {
    let mut v: Vec<Token> = Vec::new();
    let a: Vec<char> = input.chars().collect();
    let n = a.len();
    let mut i: usize = 0;
    let mut line: u32 = 0;
    let mut col: u32 = 0;
    while i<n {
        let c = a[i];
        if c.is_ascii_alphabetic() {
            let j = i;
            let col0 = col;
            while i<n && a[i].is_ascii_alphabetic() {
                i+=1; col+=1;
            }
            let id = a[j..i].iter().collect();
            if let Some(t) = is_keyword(&id) {
                v.push(Token::symbol(line,col0,*t.1));
            }else{
                v.push(Token{
                    line, col: col0,
                    symbol: Symbol::Item, item: Item::Id(id)
                });
            }
        }else if c.is_digit(10) {
            let j = i;
            let col0 = col;
            while i<n && a[i].is_digit(10) {
                i+=1; col+=1;
            }
            let sx: String = a[j..i].iter().collect();
            let x = match sx.parse::<u64>() {
                Ok(value) => value,
                Err(_) => return Err(syntax_error(line,col,
                    "could not parse number literal".into()))
            };
            v.push(Token{
                line, col: col0, symbol: Symbol::Item,
                item: Item::Int(x)
            });
        }else{
            match c {
                '\n' => {
                    i+=1; col = 0; line+=1;
                },
                ' ' => {
                    i+=1; col+=1;
                },
                '.' => {
                    v.push(Token::symbol(line,col,Symbol::Dot));
                    i+=1; col+=1;
                },
                ',' => {
                    v.push(Token::symbol(line,col,Symbol::Comma));
                    i+=1; col+=1;
                },
                ':' => {
                    if i+1<n && a[i+1]==':' {
                        v.push(Token::symbol(line,col,Symbol::PathSep));
                        i+=1; col+=1;
                    }else{
                        v.push(Token::symbol(line,col,Symbol::Colon));
                        i+=1; col+=1;
                    }
                },
                ';' => {
                    v.push(Token::symbol(line,col,Symbol::Semicolon));
                    i+=1; col+=1;
                },
                '+' => {
                    v.push(Token::symbol(line,col,Symbol::Add));
                    i+=1; col+=1;
                },
                '-' => {
                    v.push(Token::symbol(line,col,Symbol::Sub));
                    i+=1; col+=1;
                },
                '*' => {
                    v.push(Token::symbol(line,col,Symbol::Mul));
                    i+=1; col+=1;
                },
                '/' => {
                    if i+1<n && a[i+1]=='/' {
                        i+=2;
                        while i<n && a[i]!='\n' {i+=1;}
                        i+=1; line+=1; col = 0;
                    }else if i+1<n && a[i+1]=='*' {
                        i+=2; col+=2;
                        while i+1<n && (a[i]!='*' || a[i+1]!='/') {
                            if a[i]=='\n' {col = 0; line+=1;}
                            i+=1; col+=1;
                        }
                        i+=2; col+=2;
                    }else{
                        v.push(Token::symbol(line,col,Symbol::Div));
                        i+=1; col+=1;
                    }
                },
                '^' => {
                    v.push(Token::symbol(line,col,Symbol::Circumflex));
                    i+=1; col+=1;
                },
                '&' => {
                    v.push(Token::symbol(line,col,Symbol::Amp));
                    i+=1; col+=1;
                },
                '|' => {
                    v.push(Token::symbol(line,col,Symbol::Vert));
                    i+=1; col+=1;
                },
                '=' => {
                    if i+1<n && a[i+1]=='=' {
                        v.push(Token::symbol(line,col,Symbol::Eq));
                        i+=2; col+=2;
                    }else{
                        v.push(Token::symbol(line,col,Symbol::Assignment));
                        i+=1; col+=1;
                    }
                },
                '!' => {
                    if i+1<n && a[i+1]=='=' {
                        v.push(Token::symbol(line,col,Symbol::Ne));
                        i+=2; col+=2;
                    }else{
                        v.push(Token::symbol(line,col,Symbol::Not));
                        i+=1; col+=1;
                    }
                },
                '?' => {
                    v.push(Token::symbol(line,col,Symbol::Qm));
                    i+=1; col+=1;
                },
                '(' => {
                    v.push(Token::symbol(line,col,Symbol::LeftParen));
                    i+=1; col+=1;
                },
                ')' => {
                    v.push(Token::symbol(line,col,Symbol::RightParen));
                    i+=1; col+=1;
                },
                '[' => {
                    v.push(Token::symbol(line,col,Symbol::LeftBracket));
                    i+=1; col+=1;
                },
                ']' => {
                    v.push(Token::symbol(line,col,Symbol::RightBracket));
                    i+=1; col+=1;
                },
                '{' => {
                    v.push(Token::symbol(line,col,Symbol::LeftBrace));
                    i+=1; col+=1;
                },
                '}' => {
                    v.push(Token::symbol(line,col,Symbol::RightBrace));
                    i+=1; col+=1;
                },
                _ => {
                    return Err(syntax_error(line,col,format!(
                        "unexpected character: '{}'",c)));
                }
            }
        }
    }

    v.push(Token::symbol(line,col,Symbol::Terminal));
    return Ok(v);
}


struct TokenIterator<'a> {
    a: &'a [Token],
    index: Cell<usize>
}

impl<'a> TokenIterator<'a> {
    fn new(a: &'a [Token]) -> TokenIterator<'a> {
        TokenIterator{a: a, index: Cell::new(0)}
    }
    fn get(&self) -> &Token {
        return &self.a[self.index.get()];
    }
    fn advance(&self) {
        self.index.set(self.index.get()+1);
    }
}

pub enum NodeKind {
    Id(String),
    UnOp(Symbol,Rc<AST>),
    BinOp(Symbol,Rc<AST>,Rc<AST>)
}

pub struct AST {
    pub line: u32,
    pub col: u32,
    pub kind: NodeKind
}

impl AST {
    fn node(line: u32, col: u32, kind: NodeKind) -> Rc<AST> {
        Rc::new(AST{line,col,kind})
    }
}

const INDENT_SHIFT: usize = 4;

fn ast_to_string(buffer: &mut String, t: &AST, indent: usize) {
    let _ = write!(buffer,"{: <1$}","",indent);
    match &t.kind {
        NodeKind::Id(s) => {
            let _ = write!(buffer,"{}\n",s);
        },
        NodeKind::UnOp(op,x) => {
            let _ = write!(buffer,"{}\n",op.as_str());
            ast_to_string(buffer,x,indent+INDENT_SHIFT);            
        },
        NodeKind::BinOp(op,x,y) => {
            let _ = write!(buffer,"{}\n",op.as_str());
            ast_to_string(buffer,x,indent+INDENT_SHIFT);
            ast_to_string(buffer,y,indent+INDENT_SHIFT);
        }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut buffer = String::new();
        ast_to_string(&mut buffer,self,INDENT_SHIFT);
        return write!(f,"{}",buffer);
    }
}

struct Parser{}

impl Parser {

fn identifier(&mut self, i: &TokenIterator)
-> Result<Rc<AST>,SyntaxError>
{
    let t = i.get();
    if let Item::Id(ref id) = t.item {
        i.advance();
        return Ok(AST::node(t.line,t.col,NodeKind::Id(id.clone())));
    }else{
        return Err(syntax_error(t.line,t.col,
            String::from("expected identifer.")
        ));
    }
}

fn negation(&mut self, i: &TokenIterator)
-> Result<Rc<AST>,SyntaxError>
{
    let t = i.get();
    if t.symbol == Symbol::Sub {
        i.advance();
        let x = self.identifier(i)?;
        return Ok(AST::node(t.line,t.col,NodeKind::UnOp(Symbol::Neg,x)));
    }else{
        return self.identifier(i);
    }
}

fn multiplication(&mut self, i: &TokenIterator)
-> Result<Rc<AST>,SyntaxError>
{
    let mut x = self.negation(i)?;
    loop{
        let t = i.get();
        if t.symbol == Symbol::Mul  || t.symbol == Symbol::Div ||
           t.symbol == Symbol::Mod
        {
            i.advance();
            let y = self.negation(i)?;
            x = AST::node(t.line,t.col,NodeKind::BinOp(t.symbol,x,y));
        }else{
            return Ok(x);
        }
    }
}

fn addition(&mut self, i: &TokenIterator)
-> Result<Rc<AST>,SyntaxError>
{
    let mut x = self.multiplication(i)?;
    loop{
        let t = i.get();
        if t.symbol == Symbol::Add || t.symbol == Symbol::Sub {
            i.advance();
            let y = self.multiplication(i)?;
            x = AST::node(t.line,t.col,NodeKind::BinOp(t.symbol,x,y));
        }else{
            return Ok(x);
        }
    }
}

fn expression(&mut self, i: &TokenIterator)
-> Result<Rc<AST>,SyntaxError>
{
    return self.addition(i);
}

}

pub fn parse(s: &str) -> Result<Rc<AST>,SyntaxError> {
    let v = scan(s)?;
    print_vec_token(&v);
    let i = TokenIterator::new(&v);
    let mut parser = Parser{};
    let x = parser.expression(&i)?;
    println!("{}",x);
    return Ok(x);
}
