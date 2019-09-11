use std::io;

use quote::quote;

use super::Generator;
use crate::{ast::verified as ast, VERSION};

pub(self) mod builder;
pub(self) mod common;
pub(self) mod core;
pub(self) mod getter;
pub(self) mod utilities;

impl Generator {
    pub(crate) fn generate_rust<W>(&self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writeln!(writer, "// Generated by Molecule {}", VERSION)?;
        writeln!(writer)?;
        let code = quote!(
            use molecule::prelude::{Entity as _, Reader as _};
            use molecule::faster_hex::hex_string;
        );
        write!(writer, "{}", code)?;
        writeln!(writer)?;
        for decl in &self.ast.decls[..] {
            match decl.typ {
                ast::TopDeclType::Option_(ref info) => {
                    core::gen_option(writer, &decl.name, info)?;
                }
                ast::TopDeclType::Union(ref info) => {
                    core::gen_union(writer, &decl.name, info)?;
                }
                ast::TopDeclType::Array(ref info) => {
                    core::gen_array(writer, &decl.name, info)?;
                }
                ast::TopDeclType::Struct(ref info) => {
                    core::gen_struct(writer, &decl.name, info)?;
                }
                ast::TopDeclType::FixedVector(ref info) => {
                    core::gen_fix_vec(writer, &decl.name, info)?;
                }
                ast::TopDeclType::DynamicVector(ref info) => {
                    core::gen_dyn_vec(writer, &decl.name, info)?;
                }
                ast::TopDeclType::Table(ref info) => {
                    core::gen_table(writer, &decl.name, info)?;
                }
                ast::TopDeclType::Atom => unreachable!(),
            };
        }
        Ok(())
    }
}