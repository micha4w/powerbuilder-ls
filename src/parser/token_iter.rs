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

    pub ignore_newlines: bool,
}

impl<I: Iterator<Item = Token>> TokenIter<I> {
    pub fn new(iter: I) -> TokenIter<I> {
        TokenIter {
            iter,
            buf: VecDeque::new(),
            prev: None,

            ignore_newlines: false,
        }
    }

    pub fn underlying(&self) -> &I {
        &self.iter
    }

    pub fn prev(&self) -> &Option<Token> {
        &self.prev
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.peek_nth(0)
    }

    pub fn peek_nth(&mut self, mut n: usize) -> Option<Token> {
        n += 1;

        for item in &self.buf {
            if !self.ignore_newlines || item.token_type != TokenType::NEWLINE {
                n -= 1;
                if n == 0 {
                    return Some(item.clone());
                }
            }
        }

        while n > 0 {
            let item = self.iter.next()?;

            if item.token_type == TokenType::COMMENT {
                continue;
            }
            if !self.ignore_newlines || item.token_type != TokenType::NEWLINE {
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
        loop {
            let next = self.buf.pop_front().or_else(|| self.iter.next());
            if !self.ignore_newlines {
                return next;
            }
            if let Some(token) = &next {
                if token.token_type != TokenType::NEWLINE {
                    return next;
                }
            };
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (mut low, mut high) = self.iter.size_hint();
        low = low.saturating_add(self.buf.len());
        high = high.and_then(|elt| elt.checked_add(self.buf.len()));
        (low, high)
    }
}
