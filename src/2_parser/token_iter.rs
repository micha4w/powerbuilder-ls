extern crate alloc;
extern crate core as std;

use alloc::collections::VecDeque;

use crate::tokenizer::{Token, TokenType};

pub struct TokenIter<I>
where
    I: Iterator<Item = Token>,
{
    iter: I,
    buf: VecDeque<Token>,
    prev: Option<Token>,

    comments: Vec<Token>,
    // appended_comments: Vec<Token>,
    // prepended_comments: Vec<Token>,

    pub ignore_literal_newlines: bool,
}

impl<I: Iterator<Item = Token>> TokenIter<I> {
    pub fn new(iter: I) -> TokenIter<I> {
        TokenIter {
            iter,
            buf: VecDeque::new(),
            prev: None,
            comments: Vec::new(),
            // appended_comments: Vec::new(),
            // prepended_comments: Vec::new(),

            ignore_literal_newlines: false,
        }
    }

    pub fn underlying(&self) -> &I {
        &self.iter
    }

    pub fn prev(&self) -> Option<&Token> {
        self.prev.as_ref()
    }

    pub fn comments(&self) -> &Vec<Token> {
        &self.comments
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.peek_nth(0)
    }

    fn should_ignore(&self, token: &Token) -> bool {
        token.token_type == TokenType::COMMENT
            || (self.ignore_literal_newlines
                && token.token_type == TokenType::NEWLINE
                && token.content == "\n") // TODO: \r\n?
    }

    pub fn peek_nth(&mut self, mut n: usize) -> Option<Token> {
        n += 1;

        for item in &self.buf {
            if !self.should_ignore(item) {
                n -= 1;
                if n == 0 {
                    return Some(item.clone());
                }
            }
        }

        while n > 0 {
            let item = self.iter.next()?;

            if !self.should_ignore(&item) {
                n -= 1;
            }

            self.buf.push_back(item);
        }

        Some(self.buf.back().unwrap().clone())
    }
}

impl<I: Iterator<Item = Token>> Iterator for TokenIter<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next;
        loop {
            next = self.buf.pop_front().or_else(|| self.iter.next())?;
            if !self.should_ignore(&next) {
                break;
            }

            if next.token_type == TokenType::COMMENT {
                self.comments.push(next);
            }
        }

        if next.token_type == TokenType::NEWLINE
            && self.prev.as_ref().is_some_and(|prev| {
                !matches!(prev.token_type, TokenType::NEWLINE | TokenType::COMMENT)
            })
        {
            self.comments = Vec::new();
        }

        self.prev = Some(next.clone());
        Some(next)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (mut low, mut high) = self.iter.size_hint();
        low = low.saturating_add(self.buf.len());
        high = high.and_then(|elt| elt.checked_add(self.buf.len()));
        (low, high)
    }
}
