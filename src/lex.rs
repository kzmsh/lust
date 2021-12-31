#[derive(Copy, Clone, Debug)]
pub struct Location {
    col: i32,
    line: i32,
    index: usize,
}

impl Location {
    fn increment(&self, newline: bool) -> Location {
        if newline {
            Location {
                col: 0,
                line: self.line + 1,
                index: self.index + 1,
            }
        } else {
            Location {
                col: self.col + 1,
                line: self.line,
                index: self.index + 1,
            }
        }
    }

    pub fn debug<S: Into<String>>(&self, raw: &[char], msg: S) -> String {
        let mut line = 0;
        let mut line_str = String::new();

        for c in raw {
            if *c == '\n' {
                line += 1;

                if !line_str.is_empty() {
                    break;
                }

                continue;
            }

            if self.line == line {
                line_str.push_str(&c.to_string());
            }
        }

        let space = " ".repeat(self.col as usize);
        format!("{}\n\n{}\n{}^ Near here", msg.into(), line_str, space)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Identifier,
    Keyword,
    Number,
    Operator,
    Syntax,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: String,
    pub kind: TokenKind,
    pub loc: Location,
}

impl Token {
    fn new_identifier(value: String, loc: Location) -> Self {
        Self {
            value,
            loc,
            kind: TokenKind::Identifier,
        }
    }

    fn new_keyword(value: String, loc: Location) -> Self {
        Self {
            value,
            loc,
            kind: TokenKind::Keyword,
        }
    }

    fn new_number(value: String, loc: Location) -> Self {
        Self {
            value,
            loc,
            kind: TokenKind::Number,
        }
    }

    fn new_operator(value: String, loc: Location) -> Self {
        Self {
            value,
            loc,
            kind: TokenKind::Operator,
        }
    }

    fn new_syntax(value: String, loc: Location) -> Self {
        Self {
            value,
            loc,
            kind: TokenKind::Syntax,
        }
    }
}

pub fn lex(s: &[char]) -> Result<Vec<Token>, String> {
    let mut loc = Location {
        col: 0,
        line: 0,
        index: 0,
    };
    let size = s.len();
    let mut tokens: Vec<Token> = vec![];

    let lexers = [
        lex_keyword,
        lex_identifier,
        lex_number,
        lex_syntax,
        lex_operator,
    ];
    'outer: while loc.index < size {
        loc = eat_whitespace(s, loc);
        if loc.index == size {
            break;
        }

        for lexer in lexers {
            if let Some((t, next_loc)) = lexer(s, loc) {
                loc = next_loc;
                tokens.push(t);
                continue 'outer;
            }
        }

        return Err(loc.debug(s, "Unrecognized character while lexing:"));
    }

    Ok(tokens)
}

fn eat_whitespace(raw: &[char], initial_loc: Location) -> Location {
    let mut c = raw[initial_loc.index];
    let mut next_loc = initial_loc;
    while [' ', '\n', '\r', '\t'].contains(&c) {
        next_loc = next_loc.increment(c == '\n');
        if next_loc.index == raw.len() {
            break;
        }

        c = raw[next_loc.index];
    }

    next_loc
}

///
/// Any collection of alphanumeric characters and underscores, but they cannot start with a number.
///
fn lex_identifier(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    let mut ident = String::new();
    let mut next_loc = initial_loc;
    let mut c = raw[initial_loc.index];

    while c.is_alphanumeric() || c == '_' {
        ident.push_str(&c.to_string());
        next_loc = next_loc.increment(false);
        c = raw[next_loc.index];
    }

    if ident.len() > 0 && !ident.chars().next().unwrap().is_digit(10) {
        Some((Token::new_identifier(ident, initial_loc), next_loc))
    } else {
        None
    }
}

///
/// Keywords cannot be reused as variables by the user.
///
fn lex_keyword(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    // ここで定義するの何となく気に入らない
    let keywords = ["function", "end", "if", "then", "local", "return"];

    let mut next_loc = initial_loc;
    let mut value = String::new();

    'outer: for possible_keyword in keywords {
        let mut c = raw[initial_loc.index];
        next_loc = initial_loc;

        while c.is_alphanumeric() || c == '_' {
            value.push_str(&c.to_string());
            next_loc = next_loc.increment(false);
            c = raw[next_loc.index];

            let n = next_loc.index - initial_loc.index;
            if value != possible_keyword[..n] {
                value = String::new();
                continue 'outer;
            }
        }

        if value.len() < possible_keyword.len() {
            value = String::new();
            continue;
        }

        break;
    }

    if value.is_empty() {
        return None;
    }

    if next_loc.index < raw.len() - 1 {
        let next_c = raw[next_loc.index];
        if next_c.is_alphanumeric() || next_c == '_' {
            return None;
        }
    }

    Some((Token::new_keyword(value, initial_loc), next_loc))
}

///
/// This implementation only supports integers.
///
fn lex_number(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    let mut ident = String::new();
    let mut next_loc = initial_loc;
    let mut c = raw[initial_loc.index];

    while c.is_digit(10) {
        ident.push_str(&c.to_string());
        next_loc = next_loc.increment(false);
        c = raw[next_loc.index];
    }

    if !ident.is_empty() {
        Some((Token::new_number(ident, initial_loc), next_loc))
    } else {
        None
    }
}

fn lex_operator(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    let operators = ["+", "-", "<"];

    for possible_operator in operators {
        let c = raw[initial_loc.index];
        let next_loc = initial_loc.increment(false);

        if possible_operator == c.to_string() {
            return Some((
                Token::new_operator(possible_operator.to_string(), initial_loc),
                next_loc,
            ));
        }
    }

    None
}

///
/// Syntax is language junk that isn't operators.
///
fn lex_syntax(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    let syntax = [";", "=", "(", ")", ","];

    for possible_syntax in syntax {
        let c = raw[initial_loc.index];
        let next_loc = initial_loc.increment(false);

        // TODO this won't work with multiple-character syntax bits like >= or ==
        if possible_syntax == c.to_string() {
            return Some((
                Token::new_syntax(possible_syntax.to_string(), initial_loc),
                next_loc,
            ));
        }
    }

    None
}
