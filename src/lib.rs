use std::num::ParseFloatError;

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    start: usize,
    length: usize,
}

#[derive(PartialEq, Debug)]
pub enum TokenType {
    /**
     * Identifiers:
     * - Start with a letter or an underscore
     * - Contain leters, numbers, and underscores
     */
    Identifier(String),

    // Data types
    /**
     * Numbers:
     * - Start with a number
     * - Contain numbers and dot
     * - Cannot end with a dot
     * - Cannot have 2 consecutive dots
     */
    Number(f64),
    True,
    False,

    // Operators
    Equal, // =
    Plus,  // +
    Minus, // -
    Mult,  // *
    Div,   // /
    LT,    // <
    GT,    // >

    // Delimiters
    Comma,     // ,
    SemiColon, // ;
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // {

    // Reserved keywords
    Let,      // let
    Function, // fn
    If,       // if
    Else,     // else
    Return,   // return
}

#[derive(PartialEq, Debug)]
pub enum ParsingError {
    IllegalCharacterError { character: char, position: usize },
    ParseFloatError(ParseFloatError),
}

#[derive(PartialEq, Debug)]
pub struct Lexer {
    code: Vec<char>,
    position: usize,
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        Lexer {
            code: code.chars().collect(),
            position: 0,
        }
    }

    fn parse_single_character_token(&self) -> Option<Token> {
        self.code.get(self.position).and_then(|&char| {
            let token_type = match char {
                '=' => TokenType::Equal,
                '+' => TokenType::Plus,
                ',' => TokenType::Comma,
                ';' => TokenType::SemiColon,
                '(' => TokenType::LParen,
                ')' => TokenType::RParen,
                '{' => TokenType::LBrace,
                '}' => TokenType::RBrace,
                '-' => TokenType::Minus,
                '/' => TokenType::Div,
                '*' => TokenType::Mult,
                '<' => TokenType::LT,
                '>' => TokenType::GT,
                _ => return None,
            };

            Some(Token {
                start: self.position,
                length: 1,
                token_type,
            })
        })
    }

    fn parse_multi_character_token(&self) -> Result<Option<Token>, ParsingError> {
        if let Some(identifier) = self.parse_identifier() {
            let token_type = match identifier.as_str() {
                "let" => TokenType::Let,
                "fn" => TokenType::Function,
                "if" => TokenType::If,
                "else" => TokenType::Else,
                "return" => TokenType::Return,
                "true" => TokenType::True,
                "false" => TokenType::False,
                _ => TokenType::Identifier(identifier.clone()),
            };

            return Ok(Some(Token {
                start: self.position,
                token_type,
                length: identifier.len(),
            }));
        }

        if let Some(number_token) = self.parse_number()? {
            return Ok(Some(number_token));
        }

        Ok(None)
    }

    fn parse_identifier(&self) -> Option<String> {
        let &first_char = self.code.get(self.position)?;
        if !first_char.is_ascii_alphabetic() && first_char != '_' {
            return None;
        }

        let mut accumulator: String = first_char.to_string();
        let mut peak_position: usize = self.position + 1;

        while let Some(&char) = self.code.get(peak_position) {
            if !char.is_ascii_alphanumeric() && char != '_' {
                break;
            }
            accumulator.push(char);
            peak_position += 1;
        }

        Some(accumulator)
    }

    fn parse_number(&self) -> Result<Option<Token>, ParsingError> {
        if let Some(&first_char) = self.code.get(self.position) {
            if !first_char.is_ascii_digit() {
                return Ok(None);
            }

            let mut accumulator: String = first_char.to_string();
            let mut last_char: char = first_char;
            let mut peak_position: usize = self.position + 1;

            while let Some(&char) = self.code.get(peak_position) {
                if !char.is_ascii_digit() && char != '.' {
                    break;
                }

                if char == '.' && last_char == '.' {
                    return Ok(None);
                }

                accumulator.push(char);
                last_char = char;
                peak_position += 1;
            }

            if accumulator.ends_with('.') {
                return Ok(None);
            }

            let number = accumulator
                .parse::<f64>()
                .map_err(ParsingError::ParseFloatError)?;

            Ok(Some(Token {
                length: accumulator.len(),
                token_type: TokenType::Number(number),
                start: self.position,
            }))
        } else {
            Ok(None)
        }
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, ParsingError>;

    #[allow(clippy::never_loop)]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&char) = self.code.get(self.position) {
            if !char.is_whitespace() {
                break;
            }
            self.position += 1;
        }

        let mut token_result = None;

        if let Some(&char) = self.code.get(self.position) {
            if let Some(token) = self.parse_single_character_token() {
                token_result = Some(Ok(token))
            } else if let Some(token) = self.parse_multi_character_token().transpose() {
                token_result = Some(token);
            } else {
                return Some(Err(ParsingError::IllegalCharacterError {
                    character: char,
                    position: self.position,
                }));
            }
        }

        if let Some(Ok(token)) = &token_result {
            self.position += token.length;
        }

        token_result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_empty_code() {
        let lexer = Lexer::new("");
        let tokens: Vec<Token> = lexer.into_iter().map(|token| token.unwrap()).collect();
        assert_eq!(tokens.len(), 0)
    }

    #[test]
    fn parses_whitespaces() {
        let lexer = Lexer::new(" \t\n");
        let tokens: Vec<Token> = lexer.into_iter().map(|token| token.unwrap()).collect();
        assert_eq!(tokens.len(), 0)
    }

    #[test]
    fn returns_error_if_parsing_fails() {
        let lexer = Lexer::new("123..456");
        let tokens: Result<Vec<Token>, ParsingError> = lexer.into_iter().collect();
        assert!(tokens.is_err());

        let lexer = Lexer::new("123.");
        let tokens: Result<Vec<Token>, ParsingError> = lexer.into_iter().collect();
        assert!(tokens.is_err());

        let lexer = Lexer::new("`");
        let tokens: Result<Vec<Token>, ParsingError> = lexer.into_iter().collect();
        assert!(tokens.is_err());
    }

    #[test]
    fn recognises_delimiters() {
        let lexer = Lexer::new(",;(){}");
        let tokens: Vec<TokenType> = lexer
            .into_iter()
            .map(|token| token.unwrap().token_type)
            .collect();
        assert_eq!(
            tokens,
            [
                TokenType::Comma,
                TokenType::SemiColon,
                TokenType::LParen,
                TokenType::RParen,
                TokenType::LBrace,
                TokenType::RBrace,
            ]
        )
    }

    #[test]
    fn recognises_operators() {
        let input = [
            ("=", TokenType::Equal),
            ("+", TokenType::Plus),
            ("-", TokenType::Minus),
            ("*", TokenType::Mult),
            ("/", TokenType::Div),
            ("<", TokenType::LT),
            (">", TokenType::GT),
        ];

        input.into_iter().for_each(|(code, expected)| {
            let lexer = Lexer::new(code);
            let tokens: Vec<TokenType> = lexer
                .into_iter()
                .map(|token| token.unwrap().token_type)
                .collect();
            assert_eq!(tokens, [expected])
        });
    }

    #[test]
    fn recognises_keywords() {
        let input = [
            ("let", TokenType::Let),
            ("fn", TokenType::Function),
            ("if", TokenType::If),
            ("else", TokenType::Else),
            ("return", TokenType::Return),
            ("true", TokenType::True),
            ("false", TokenType::False),
        ];

        input.into_iter().for_each(|(code, expected)| {
            let lexer = Lexer::new(code);
            let tokens: Vec<TokenType> = lexer
                .into_iter()
                .map(|token| token.unwrap().token_type)
                .collect();
            assert_eq!(tokens, [expected])
        });
    }

    #[test]
    fn recognises_identifiers() {
        let input = [
            ("var", TokenType::Identifier("var".into())),
            ("_var", TokenType::Identifier("_var".into())),
            ("var2", TokenType::Identifier("var2".into())),
        ];

        input.into_iter().for_each(|(code, expected)| {
            let lexer = Lexer::new(code);
            let tokens: Vec<TokenType> = lexer
                .into_iter()
                .map(|token| token.unwrap().token_type)
                .collect();
            assert_eq!(tokens, [expected])
        });
    }

    #[test]
    fn recognises_numbers() {
        let input = [
            ("123", TokenType::Number(123.0)),
            ("123.456", TokenType::Number(123.456)),
        ];

        input.into_iter().for_each(|(code, expected)| {
            let lexer = Lexer::new(code);
            let tokens: Vec<TokenType> = lexer
                .into_iter()
                .map(|token| token.unwrap().token_type)
                .collect();
            assert_eq!(tokens, [expected])
        });
    }
}
