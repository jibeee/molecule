use std::io;

use super::utilities::IdentPrefix;
use crate::ast;

pub(super) trait GenReader: IdentPrefix {
    fn gen_reader_interfaces_internal<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        {
            w!(writer, "struct {name}_state;", name = self.name());
            w!(writer, "struct {name}_callbacks;", name = self.name());
            w!(
                writer,
                "typedef const struct {name}_callbacks {name}_cb;",
                name = self.name()
            );
            self.function_signature(
                writer,
                "_init_state",
                format!(
                    "(struct {name}_state *s, const struct {name}_callbacks *cb)",
                    name = self.name()
                )
                .as_str(),
                "void",
                ";",
            )?;
            self.function_signature(writer, "_parse", format!("(struct {name}_state *s, struct mol_chunk *chunk, const struct {name}_callbacks *cb, mol_num_t size)", name=self.name()).as_str(), "mol_rv", ";")?;
            w!(writer, "");
        }
        Ok(())
    }

    fn gen_reader_interfaces<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.gen_reader_interfaces_internal(writer)?;
        Ok(())
    }

    fn gen_reader_functions<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.gen_reader_function_verify(writer)?;
        Ok(())
    }

    fn gen_reader_function_verify<W: io::Write>(&self, _writer: &mut W) -> io::Result<()>;
    fn gen_reader_structs<W: io::Write>(&self, _writer: &mut W) -> io::Result<()>;
}

impl GenReader for ast::Option_ {
    fn gen_reader_structs<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            w!(o, "struct {}_state {{", self.name());
            let f = self.item();
            w!(
                o,
                "    union {{ struct {}_state item; struct num_state num; }} u;",
                f.typ().get_name()
            );
            w!(o, "}};");
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*end)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(
                o,
                "    const struct {}_callbacks *item;",
                f.typ().get_name()
            );
            w!(o, "}};");
            w!(o, "");
        }
        Ok(())
    }
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            self.start_init_function(o)?;
            w!(
                o,
                "    MOL_INIT_SUBPARSER(item, {field_type});",
                field_type = self.item().typ().get_name()
            );
            w!(o, "}}");
            w!(o, "");
            self.start_parser_function(o)?;
            w!(o, "    // Option");
            w!(o, "    if(size==0) {{");
            self.success(o)?;
            w!(o, "    }}");
            let f = self.item();
            let fty = f.typ().get_name();
            w!(o, "    MOL_CALL_SUBPARSER(item, {}, size)", fty);
            self.success(o)?;
            w!(o, "}}");
        }
        Ok(())
    }
}

impl GenReader for ast::Union {
    fn gen_reader_interfaces_internal<W: io::Write>(&self, _writer: &mut W) -> io::Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "Union is unimplemented",
        ))
    }

    fn gen_reader_structs<W: io::Write>(&self, _o: &mut W) -> io::Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "Union is unimplemented",
        ))
    }

    fn gen_reader_function_verify<W: io::Write>(&self, _o: &mut W) -> io::Result<()> {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "Union is unimplemented",
        ))
    }
}

impl GenReader for ast::Array {
    fn gen_reader_structs<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        if self.item().typ().is_byte() {
            w!(o, "struct {}_state {{", self.name());
            w!(o, "    struct bytes_state state;");
            w!(o, "}};");
            w!(
                o,
                "struct {}_callbacks {{ struct bytes_callbacks cb; }};",
                self.name()
            );
        } else {
            w!(o, "struct {}_state {{", self.name());
            w!(o, "    mol_num_t state_num;");
            let f = self.item();
            w!(
                o,
                "    union {{ struct {}_state item; struct num_state num; }} u;",
                f.typ().get_name()
            );
            w!(o, "}};");
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(o, "    void (*end)();");
            w!(
                o,
                "    const struct item *{};",
                self.item().typ().get_name()
            );
            w!(o, "}};");
        }
        Ok(())
    }
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        if self.item().typ().is_byte() {
            let i_macro_sig_tail = "_init_state(g, cbs)";
            let i_macro_content = "mol_bytes_init_state(&(g->state), &((cbs)->cb))";
            self.define_reader_macro(o, &i_macro_sig_tail, &i_macro_content)?;
            let macro_sig_tail = "_parse(g, c, cbs, sz)";
            let macro_content = format!(
                "mol_parse_bytes(&(g->state), c, &((cbs)->cb), {})",
                self.item_count()
            );
            self.define_reader_macro(o, &macro_sig_tail, &macro_content)?;
        } else {
            self.start_init_function(o)?;
            w!(o, "        state->state_num=0; ");
            w!(
                o,
                "        MOL_INIT_SUBPARSER(item, {field_type});",
                field_type = self.item().typ().get_name()
            );
            w!(o, "    }} ");
            self.start_parser_function(o)?;
            w!(
                o,
                "    if(size != {} && size != MOL_NUM_MAX) return REJECT;",
                self.item_count() * self.item_size()
            );
            w!(
                o,
                "    while(state->state_num < {length}) {{ ",
                length = self.item_count()
            );
            w!(
                o,
                "        MOL_CALL_SUBPARSER(item, {}, {})",
                self.item().typ().get_name(),
                self.item_size()
            );
            w!(
                o,
                "        MOL_INIT_SUBPARSER(item, {field_type});",
                field_type = self.item().typ().get_name()
            );
            w!(o, "        state->state_num++; ");
            w!(o, "    }}");
            self.success(o)?;
            w!(o, "    return INCOMPLETE;");
            w!(o, "}}");
        }
        Ok(())
    }
}

impl GenReader for ast::Struct {
    fn gen_reader_structs<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            w!(o, "struct {}_state {{", self.name());
            w!(o, "    mol_num_t state_num;");
            w!(o, "    union {{");
            for (f, _field_size) in self.fields().iter().zip(self.field_sizes().iter()) {
                w!(
                    o,
                    "        struct {}_state {};",
                    f.typ().get_name(),
                    f.name()
                );
            }
            w!(o, "        struct num_state num;");
            w!(o, "    }} u;");
            w!(o, "}};");
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(o, "    void (*end)();");
            for (f, _field_size) in self.fields().iter().zip(self.field_sizes().iter()) {
                w!(
                    o,
                    "    const struct {}_callbacks *{};",
                    f.typ().get_name(),
                    f.name()
                );
            }
            w!(o, "}};");
            w!(o, "");
            Ok(())
        }
    }
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            self.start_init_function(o)?;
            w!(o, "    s->state_num=0;");
            match self.fields().first() {
                None => {}
                Some(f) => w!(
                    o,
                    "    MOL_INIT_SUBPARSER({field_name}, {field_type});",
                    field_name = f.name(),
                    field_type = f.typ().get_name()
                ),
            }
            w!(o, "}}");
            w!(o, "");
            self.start_parser_function(o)?;
            w!(o, "    // Struct");
            w!(
                o,
                "    if(size != {} && size != MOL_NUM_MAX) return REJECT;",
                self.total_size()
            );
            w!(o, "    switch(s->state_num) {{");
            for (i, ((f, nf), fsz)) in self
                .fields()
                .iter()
                .zip(self.fields().iter().skip(1))
                .zip(self.field_sizes().iter())
                .enumerate()
            {
                let fty = f.typ().get_name();
                let nfty = nf.typ().get_name();
                w!(o, "        case {}: {{", i);
                w!(
                    o,
                    "            MOL_CALL_SUBPARSER({field_name}, {field_type}, {field_size})",
                    field_name = f.name(),
                    field_type = fty,
                    field_size = fsz
                );
                w!(o, "            s->state_num++;");
                w!(
                    o,
                    "            MOL_INIT_SUBPARSER({field_name}, {field_type})",
                    field_name = nf.name(),
                    field_type = nfty
                );
                w!(o, "        }}");
            }
            match self.fields().iter().zip(self.field_sizes().iter()).last() {
                None => Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Empty table is unimplemented",
                )),
                Some((lf, lfsz)) => {
                    let lfty = lf.typ().get_name();
                    w!(o, "        case {}: {{", self.fields().len() - 1);
                    w!(
                        o,
                        "            MOL_CALL_SUBPARSER({field_name}, {field_type}, {field_size})",
                        field_name = lf.name(),
                        field_type = lfty,
                        field_size = lfsz
                    );
                    w!(o, "        }}");
                    w!(o, "    }}");
                    self.success(o)?;
                    w!(o, "}}");
                    w!(o, "");
                    Ok(())
                }
            }
        }
    }
}

impl GenReader for ast::FixVec {
    fn gen_reader_structs<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            w!(o, "struct {}_state {{", self.name());
            w!(o, "    mol_num_t state_num;");
            w!(o, "    mol_num_t length;");
            let f = self.item();
            w!(
                o,
                "    union {{ struct {}_state item; struct num_state num;}} u;",
                f.typ().get_name()
            );
            w!(o, "}};");
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(o, "    void (*end)();");
            w!(o, "    void (*size)(mol_num_t);");
            if !self.item().typ().is_byte() {
                w!(o, "    void (*index)(mol_num_t);");
                w!(
                    o,
                    "    const struct {}_callbacks *item;",
                    self.item().typ().get_name()
                );
            } else {
                w!(o, "    void (*body_chunk)(uint8_t*, mol_num_t);");
            }
            w!(o, "}};");
            w!(o, "");
            self.start_init_function(o)?;
            w!(o, "    s->state_num=0;");
            w!(o, "    MOL_INIT_NUM();");
            w!(o, "}}");
        }
        Ok(())
    }

    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            self.start_parser_function(o)?;
            w!(o, "    if(s->state_num == 0) {{");
            w!(o, "        MOL_CALL_NUM(s->length);");
            w!(o, "        s->state_num++;");
            w!(
                o,
                "        if(size != MOL_NUM_MAX && (s->length * {} + 4) != size) return REJECT;",
                self.item_size()
            );
            w!(
                o,
                "        if(cb && cb->size) MOL_PIC(cb->size)(s->length * {} + 4);",
                self.item_size()
            );
            if !self.item().typ().is_byte() {
                w!(
                    o,
                    "        MOL_INIT_SUBPARSER(item, {});",
                    self.item().typ().get_name()
                );
                w!(
                    o,
                    "        if(cb && cb->index) MOL_PIC(cb->index)(s->state_num-1);"
                );
            }
            w!(o, "    }}");
            if self.item().typ().is_byte() {
                w!(o, "    mol_num_t needed=s->length+1-s->state_num;");
                w!(o, "    mol_num_t available=chunk->length-chunk->consumed;");
                w!(o, "    mol_num_t copy=needed<available?needed:available;");
                w!(o, "    if(needed && cb && cb->body_chunk) MOL_PIC(cb->body_chunk)(chunk->ptr + chunk->consumed, copy);");
                w!(o, "    chunk->consumed+=copy;");
                w!(o, "    s->state_num+=copy;");
                w!(o, "    if(s->state_num-1 < s->length) return INCOMPLETE;");
                w!(o, "    DONE();");
            } else {
                w!(o, "    while(s->state_num-1 < s->length) {{");
                w!(
                    o,
                    "        MOL_CALL_SUBPARSER(item, {}, {});",
                    self.item().typ().get_name(),
                    self.item_size()
                );
                w!(
                    o,
                    "        MOL_INIT_SUBPARSER(item, {});",
                    self.item().typ().get_name()
                );
                w!(o, "        s->state_num++;");
                w!(
                    o,
                    "        if(cb && cb->index) MOL_PIC(cb->index)(s->state_num-1);"
                );
                w!(o, "    }}");
                w!(o, "    DONE();");
            }
            w!(o, "}}");
        }
        Ok(())
    }
}

impl GenReader for ast::DynVec {
    fn gen_reader_structs<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        // Only supporting DynVec of Table and DynVec of FixVec
        // Start without length validation.
        {
            w!(o, "struct {}_state {{", self.name());
            w!(o, "    mol_num_t state_num;");
            w!(o, "    mol_num_t field_idx;");
            w!(o, "    mol_num_t total_size;");
            w!(o, "    mol_num_t first_offset;");
            let f = self.item();
            w!(
                o,
                "    union {{ struct {}_state item; struct num_state num;}} u;",
                f.typ().get_name()
            );
            w!(o, "}};");
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(o, "    void (*end)();");
            w!(o, "    void (*size)(mol_num_t);");
            w!(o, "    void (*length)(mol_num_t);");
            w!(o, "    void (*index)(mol_num_t);");
            w!(o, "    void (*offset)(mol_num_t);");
            w!(
                o,
                "    const struct {}_callbacks *item;",
                self.item().typ().get_name()
            );
            w!(o, "}};");
            w!(o, "");
            Ok(())
        }
    }
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            self.start_init_function(o)?;
            w!(o, "    s->state_num=0;");
            w!(o, "    s->field_idx=0;");
            w!(o, "    MOL_INIT_NUM();");
            w!(o, "}}");
            w!(o, "");
            self.start_parser_function(o)?;
            w!(o, "    // DynVec");
            w!(o, "    (void)(size); // FIXME: check sizes.");
            w!(o, "    switch(s->state_num) {{");
            w!(o, "        case 0:");
            w!(o, "            MOL_CALL_NUM(s->total_size);");
            w!(o, "            MOL_INIT_NUM();");
            w!(
                o,
                "            if(cb && cb->size) MOL_PIC(cb->size)(s->total_size);"
            );
            w!(o, "            if(s->total_size==4) {{");
            self.success(o)?;
            w!(o, "            }}");
            w!(
                o,
                "            if(size != MOL_NUM_MAX && size != s->total_size) return REJECT;"
            );
            w!(o, "            s->state_num++;");
            w!(o, "        case 1:");
            w!(o, "            MOL_CALL_NUM(s->first_offset);");
            w!(o, "            MOL_INIT_NUM();");
            w!(o, "            s->field_idx=1;");
            w!(o, "            s->state_num++;");
            w!(o, "        case 2:");
            w!(
                o,
                "            while(s->field_idx < (s->first_offset>>2)-1) {{ "
            );
            w!(o, "                mol_num_t scratch;");
            w!(o, "                MOL_CALL_NUM(scratch);");
            w!(o, "                MOL_INIT_NUM();");
            w!(o, "                s->field_idx++;");
            // TODO: ensure equality of item-internal and item-external sizes.
            w!(o, "            }}");
            let fty = self.item().typ().get_name();
            w!(
                o,
                "            MOL_INIT_SUBPARSER(item, {field_type})",
                field_type = fty
            );
            w!(o, "            s->field_idx=0;");
            w!(o, "            s->state_num++;");
            w!(
                o,
                "            if(cb && cb->length) MOL_PIC(cb->length)((s->first_offset>>2)-1);"
            );
            w!(
                o,
                "            if(cb && cb->index) MOL_PIC(cb->index)(s->field_idx);"
            );
            w!(o, "        case 3:");
            w!(
                o,
                "            while(s->field_idx < (s->first_offset>>2)-1) {{ "
            );
            w!(
                o,
                "                MOL_CALL_SUBPARSER(item, {field_type}, -1)",
                field_type = fty
            );
            w!(
                o,
                "                MOL_INIT_SUBPARSER(item, {field_type})",
                field_type = fty
            );
            w!(o, "                s->field_idx++;");
            w!(
                o,
                "                if(cb && cb->index) MOL_PIC(cb->index)(s->field_idx);"
            );
            w!(o, "            }}");
            w!(o, "    }}");
            self.success(o)?;
            w!(o, "}}");
        }
        Ok(())
    }
}

impl GenReader for ast::Table {
    fn gen_reader_structs<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            w!(o, "struct {}_state {{", self.name());
            w!(o, "    mol_num_t state_num;");
            w!(o, "    mol_num_t field_idx;");
            w!(o, "    mol_num_t total_size;");
            for f in self.fields().iter() {
                w!(o, "    mol_num_t {fn}_offset;", fn=f.name());
            }
            w!(o, "    union {{");
            for f in self.fields().iter() {
                w!(
                    o,
                    "        struct {}_state {};",
                    f.typ().get_name(),
                    f.name()
                );
            }
            w!(o, "        struct num_state num;");
            w!(o, "    }} u;");
            w!(o, "}};");
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(o, "    void (*end)();");
            w!(o, "    void (*offsets)(struct {}_state*);", self.name());
            for f in self.fields().iter() {
                w!(
                    o,
                    "    const struct {}_callbacks *{};",
                    f.typ().get_name(),
                    f.name()
                );
            }
            w!(o, "}};");
            w!(o, "");
        }
        Ok(())
    }
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            self.start_init_function(o)?;
            w!(o, "    s->state_num=0;");
            w!(o, "    s->field_idx=0;");
            w!(o, "    MOL_INIT_NUM();");
            w!(o, "}}");
            w!(o, "");
            self.start_parser_function(o)?;
            w!(o, "    // Table");
            w!(o, "    switch(s->state_num) {{");
            w!(o, "        case 0:");
            w!(o, "            MOL_CALL_NUM(s->total_size);");
            w!(o, "            MOL_INIT_NUM();");
            w!(
                o,
                "            if(size!=MOL_NUM_MAX && s->total_size != size) return REJECT;"
            );
            w!(o, "            s->state_num++;");
            w!(o, "        case 1:");
            w!(o, "            switch(s->field_idx) {{");
            for (i, f) in self.fields().iter().enumerate() {
                w!(o, "                case {}: {{", i);
                w!(
                    o,
                    "                    MOL_CALL_NUM(s->{}_offset);",
                    f.name()
                );
                w!(o, "                    MOL_INIT_NUM();");
                w!(o, "                    s->field_idx++;");
                w!(o, "                }}");
            }
            w!(o, "            }}");
            w!(o, "            s->state_num++;");
            w!(o, "            s->field_idx=0;");
            w!(
                o,
                "            MOL_INIT_SUBPARSER({}, {});",
                self.fields()[0].name(),
                self.fields()[0].typ().get_name()
            ); // fails on empty table.
            w!(
                o,
                "            if(cb && cb->offsets) MOL_PIC(cb->offsets)(s);"
            );
            w!(o, "        case 2:");
            w!(o, "            switch(s->field_idx) {{");
            for (i, (f, nf)) in self
                .fields()
                .iter()
                .zip(self.fields().iter().skip(1))
                .enumerate()
            {
                let fty = f.typ().get_name();
                let nfty = nf.typ().get_name();
                w!(o, "            case {}: {{", i);
                w!(
                    o,
                    "                MOL_CALL_SUBPARSER({}, {}, s->{}_offset - s->{}_offset);",
                    f.name(),
                    fty,
                    nf.name(),
                    f.name()
                );
                w!(
                    o,
                    "                MOL_INIT_SUBPARSER({}, {});",
                    nf.name(),
                    nfty
                );
                w!(o, "                s->field_idx++;");
                w!(o, "            }}");
            }
            match self.fields().last() {
                None => Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Empty table is unimplemented",
                )),
                Some(lf) => {
                    let lfty = lf.typ().get_name();
                    w!(o, "            case {}: {{", self.fields().len() - 1);
                    w!(
                        o,
                        "                MOL_CALL_SUBPARSER({}, {}, s->total_size-s->{}_offset);",
                        lf.name(),
                        lfty,
                        lf.name()
                    );
                    w!(o, "                s->state_num++;");
                    w!(o, "            }}");
                    w!(o, "        }}");
                    w!(o, "    }}");
                    self.success(o)?;
                    w!(o, "}}\n");
                    Ok(())
                }
            }
        }
    }
}

impl GenReader for ast::TopDecl {
    fn gen_reader_interfaces_internal<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            ast::TopDecl::Option_(ref i) => i.gen_reader_interfaces_internal(writer),
            ast::TopDecl::Union(ref i) => i.gen_reader_interfaces_internal(writer),
            ast::TopDecl::Array(ref i) => i.gen_reader_interfaces_internal(writer),
            ast::TopDecl::Struct(ref i) => i.gen_reader_interfaces_internal(writer),
            ast::TopDecl::FixVec(ref i) => i.gen_reader_interfaces_internal(writer),
            ast::TopDecl::DynVec(ref i) => i.gen_reader_interfaces_internal(writer),
            ast::TopDecl::Table(ref i) => i.gen_reader_interfaces_internal(writer),
            ast::TopDecl::Primitive(_) => unreachable!(),
        }
    }

    fn gen_reader_function_verify<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            ast::TopDecl::Option_(ref i) => i.gen_reader_function_verify(writer),
            ast::TopDecl::Union(ref i) => i.gen_reader_function_verify(writer),
            ast::TopDecl::Array(ref i) => i.gen_reader_function_verify(writer),
            ast::TopDecl::Struct(ref i) => i.gen_reader_function_verify(writer),
            ast::TopDecl::FixVec(ref i) => i.gen_reader_function_verify(writer),
            ast::TopDecl::DynVec(ref i) => i.gen_reader_function_verify(writer),
            ast::TopDecl::Table(ref i) => i.gen_reader_function_verify(writer),
            ast::TopDecl::Primitive(_) => unreachable!(),
        }
    }
    fn gen_reader_structs<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            ast::TopDecl::Option_(ref i) => i.gen_reader_structs(writer),
            ast::TopDecl::Union(ref i) => i.gen_reader_structs(writer),
            ast::TopDecl::Array(ref i) => i.gen_reader_structs(writer),
            ast::TopDecl::Struct(ref i) => i.gen_reader_structs(writer),
            ast::TopDecl::FixVec(ref i) => i.gen_reader_structs(writer),
            ast::TopDecl::DynVec(ref i) => i.gen_reader_structs(writer),
            ast::TopDecl::Table(ref i) => i.gen_reader_structs(writer),
            ast::TopDecl::Primitive(_) => unreachable!(),
        }
    }
}
