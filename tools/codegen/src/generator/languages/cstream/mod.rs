use std::io;

use case::CaseExt;

use crate::{ast, C_API_VERSION_MIN, VERSION};

#[macro_use]
pub(self) mod utilities;

mod import;

mod reader;

use self::{import::GenImport, reader::GenReader};

pub(crate) struct Generator;

impl Generator {
    fn title<W: io::Write>(writer: &mut W, title: &str) -> io::Result<()> {
        writeln!(writer, "/*")?;
        writeln!(writer, " * {}", title)?;
        writeln!(writer, " */")?;
        writeln!(writer)
    }

    fn ifndef<W: io::Write>(o: &mut W, name: &str) -> io::Result<()> {
        let n = name.to_snake().to_uppercase();
        let api_decorator = utilities::API_DECORATOR;
        w!(o, "#ifndef {}_H                                        ", n);
        w!(o, "#define {}_H                                        ", n);
        w!(o, "                                                       ");
        w!(o, "#ifdef __cplusplus                                     ");
        w!(o, "#define _CPP_BEGIN extern \"C\" {{                     ");
        w!(o, "#define _CPP_END }}                                    ");
        w!(o, "_CPP_BEGIN                                             ");
        w!(o, "#endif /* __cplusplus */                               ");
        w!(o, "                                                       ");
        w!(o, "#ifndef {}                              ", api_decorator);
        w!(o, "#define __DEFINE_MOLECULE_API_DECORATOR_{}          ", n);
        w!(o, "#define {}                              ", api_decorator);
        w!(o, "#endif /* {} */                         ", api_decorator);
        Ok(())
    }

    fn endif<W: io::Write>(o: &mut W, name: &str) -> io::Result<()> {
        let n = name.to_snake().to_uppercase();
        let api_decorator = utilities::API_DECORATOR;
        w!(o, "                                                       ");
        w!(o, "#ifdef __DEFINE_MOLECULE_API_DECORATOR_{}           ", n);
        w!(o, "#undef {}                               ", api_decorator);
        w!(o, "#undef __DEFINE_MOLECULE_API_DECORATOR_{}           ", n);
        w!(o, "#endif /* __DEFINE_MOLECULE_API_DECORATOR_{} */     ", n);
        w!(o, "                                                       ");
        w!(o, "#ifdef __cplusplus                                     ");
        w!(o, "_CPP_END                                               ");
        w!(o, "#undef _CPP_BEGIN                                      ");
        w!(o, "#undef _CPP_END                                        ");
        w!(o, "#endif /* __cplusplus */                               ");
        w!(o, "                                                       ");
        w!(o, "#endif /* {}_H */                                   ", n);
        Ok(())
    }

    fn define_version<W: io::Write>(o: &mut W) -> io::Result<()> {
        let molc_ver = semver::Version::parse(VERSION)
            .map(|v| (v.major * 1000 + v.minor) * 1000 + v.patch)
            .unwrap();
        let api_ver_min = semver::Version::parse(C_API_VERSION_MIN)
            .map(|v| (v.major * 1000 + v.minor) * 1000 + v.patch)
            .unwrap();
        w!(o, "#define MOLECULEC_VERSION {}                 ", molc_ver);
        w!(o, "#define MOLECULE_API_VERSION_MIN {}       ", api_ver_min);
        Ok(())
    }
}

impl super::LanguageGenerator for Generator {
    fn generate<W: io::Write>(writer: &mut W, ast: &ast::Ast) -> io::Result<()> {
        writeln!(writer, "// Generated by Molecule {}", VERSION)?;
        writeln!(writer)?;
        Self::define_version(writer)?;
        writeln!(writer)?;
        writeln!(writer, r#"#include "molecule_reader_streaming.h""#)?;
        //writeln!(writer, r#"#include "molecule_builder.h""#)?;
        writeln!(writer)?;
        Self::ifndef(writer, &ast.namespace())?;
        let imports = ast.imports();
        if !imports.is_empty() {
            writeln!(writer)?;
            for import in imports {
                import.gen_import(writer)?;
            }
        }
        writeln!(writer)?;
        Self::title(writer, "Reader APIs")?;
        for decl in ast.major_decls() {
            decl.gen_reader_structs(writer)?;
        }
        for decl in ast.major_decls() {
            decl.gen_reader_interfaces(writer)?;
        }
        /*writeln!(writer)?;
        Self::title(writer, "Builder APIs")?;
        for decl in ast.major_decls() {
            decl.gen_builder_interfaces(writer)?;
        }
        writeln!(writer)?;
        Self::title(writer, "Default Value")?;
        writeln!(writer, r#"#define ____ 0x00"#)?;
        writeln!(writer)?;
        for decl in ast.major_decls() {
            decl.gen_default(writer)?;
        } */
        writeln!(writer)?;
        writeln!(writer, r#"#undef ____"#)?;
        writeln!(writer)?;
        Self::title(writer, "Reader Functions")?;
        for decl in ast.major_decls() {
            decl.gen_reader_functions(writer)?;
        }
        /*
        writeln!(writer)?;
        Self::title(writer, "Builder Functions")?;
        for decl in ast.major_decls() {
            decl.gen_builder_functions(writer)?;
        }
        */
        Self::endif(writer, &ast.namespace())?;
        Ok(())
    }
}
