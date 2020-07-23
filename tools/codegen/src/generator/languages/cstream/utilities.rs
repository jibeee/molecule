use std::io;

use crate::ast::{self as ast, HasName};

pub(super) const API_DECORATOR: &str = "MOLECULE_API_DECORATOR";

macro_rules! w {
    ($writer:ident, $( $args:tt )*) => {
        writeln!($writer, "{}", format!($( $args )*).trim_end())?;
    }
}

pub(super) trait IdentPrefix: HasName {
    fn get_name(&self) -> String {
        self.name().to_string()
    }

    fn reader_prefix(&self) -> String {
        format!("MolReader_{}", self.name())
    }

    fn api_decorator(&self) -> &str {
        API_DECORATOR
    }

    fn define_reader_macro<W: io::Write>(
        &self,
        writer: &mut W,
        macro_sig_tail: &str,
        macro_content: &str,
    ) -> io::Result<()> {
        self.define_macro(true, writer, macro_sig_tail, macro_content)
    }

    fn define_macro<W: io::Write>(
        &self,
        is_reader: bool,
        writer: &mut W,
        macro_sig_tail: &str,
        macro_content: &str,
    ) -> io::Result<()> {
        let prefix = self.reader_prefix();
        let macro_sig = format!("{}{}", prefix, macro_sig_tail);
        writeln!(
            writer,
            "{:39} {:47} {}",
            "#define", macro_sig, macro_content
        )
    }

    fn function_signature<W: io::Write>(
        &self,
        writer: &mut W,
        func_sig_tail: &str,
        func_args: &str,
        func_ret: &str,
        end: &str
    ) -> io::Result<()> {
        let func_name = format!("{}{}", self.reader_prefix(), func_sig_tail);
        writeln!(
            writer,
            "{:23} {:15} {:47} {}{}",
            self.api_decorator(),
            func_ret,
            func_name,
            func_args,
            end
        )
    }
    
    fn start_parser_function<W: io::Write>(
        &self,
        writer: &mut W,
    ) -> io::Result<()> {
        self.function_signature(writer, "_parse", format!("(struct {name}_state *s, struct mol_chunk *chunk, const struct {name}_callbacks *cb, mol_num_t size)", name=self.name()).as_str(), "mol_rv", " {");
        w!(writer, "    mol_num_t start_idx = chunk->consumed;");
        Ok(())
    }
    
    fn start_init_function<W: io::Write>(
        &self,
        writer: &mut W,
    ) -> io::Result<()> {
        self.function_signature(writer, "_init_state", format!("(struct {name}_state *s, const struct {name}_callbacks *cb)", name=self.name()).as_str(), "void", " {");
        w!(writer, "    if(cb && cb->start) MOL_PIC(cb->start)();");
        Ok(())
    }

    fn success<W: io::Write>(
        &self,
        writer: &mut W,
    ) -> io::Result<()> {
		w!(writer, "    if(cb && cb->chunk) MOL_PIC(cb->chunk)(chunk->ptr + start_idx, chunk->consumed - start_idx);");
        w!(writer, "    if(cb && cb->end) MOL_PIC(cb->end)();");
        w!(writer, "    return COMPLETE;");
        Ok(())
    }

}

impl IdentPrefix for ast::Option_ {}

impl IdentPrefix for ast::Union {}

impl IdentPrefix for ast::Array {}

impl IdentPrefix for ast::Struct {}

impl IdentPrefix for ast::FixVec {}

impl IdentPrefix for ast::DynVec {}

impl IdentPrefix for ast::Table {}

impl IdentPrefix for ast::TopDecl {}
