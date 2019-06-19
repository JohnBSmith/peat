
#[derive(Clone,Copy)]
pub enum Symbol {
    Terminal, Item, Plus, Minus, Ast, Slash, Circumflex, Amp, Vert,
    Assignment, Eq, Ne, Not, Qm,
    Dot, Comma, Colon, Semicolon,
    LeftParen, RightParen,
    LeftBracket, RightBracket,
    LeftBrace, RightBrace,

    /* Keywords */
    And, Break, Continue,
    Else, Enum, False, Fn, For, If, In, Let, Loop,
    Match, Mod, Mut,
    Or, Pub, Return, Struct, Trait, True, Type,
    Use, Where, While,
}

pub enum Item {
    None, Int(u64), Id(String),
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
    pub text: String,
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
    ("while",   &Symbol::While),
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
    buffer.push_str(match t.symbol {
        Symbol::Terminal => "Terminal",
        Symbol::Item => {
            item_to_string(buffer,&t.item);
            return;
        },
        Symbol::Plus => "+",
        Symbol::Minus => "-",
        Symbol::Ast => "*",
        Symbol::Slash => "/",
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
        Symbol::While => "while",
    })
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
                    v.push(Token::symbol(line,col,Symbol::Colon));
                    i+=1; col+=1;
                },
                ';' => {
                    v.push(Token::symbol(line,col,Symbol::Semicolon));
                    i+=1; col+=1;
                },
                '+' => {
                    v.push(Token::symbol(line,col,Symbol::Plus));
                    i+=1; col+=1;
                },
                '-' => {
                    v.push(Token::symbol(line,col,Symbol::Minus));
                    i+=1; col+=1;
                },
                '*' => {
                    v.push(Token::symbol(line,col,Symbol::Ast));
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
                        v.push(Token::symbol(line,col,Symbol::Slash));
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

