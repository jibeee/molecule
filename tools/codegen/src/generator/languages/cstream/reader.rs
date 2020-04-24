use std::io;

use super::utilities::IdentPrefix;
use crate::ast;

pub(super) trait GenReader: IdentPrefix {
    fn gen_reader_interfaces_internal<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        {
            w!(writer, "struct {name}_state;", name=self.name());
            w!(writer, "struct {name}_callbacks;", name=self.name());
            self.function_signature(writer, "_init_state", format!("(struct {name}_state *s, struct mol_chunk *chunk, struct {name}_callbacks *cb)", name=self.name()).as_str(), "void", ";");
            self.function_signature(writer, "_parse", format!("(struct {name}_state *s, struct mol_chunk *chunk, struct {name}_callbacks *cb, mol_num_t size)", name=self.name()).as_str(), "mol_rv", ";");
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
}

impl GenReader for ast::Option_ {
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            w!(o, "struct {}_state {{ }};", self.name());
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*end)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            let f=self.item();
            w!(o, "    struct {}_callbacks *item;", f.typ().get_name());
            w!(o, "}};");
            w!(o, "");
        }
        {
            self.start_init_function(o);
            w!(o, "    MOL_INIT_SUBPARSER(item, {field_type});", field_type=self.item().typ().get_name());
            w!(o, "}}");
            w!(o, "");
            self.start_parser_function(o);
            w!(o, "    mol_rv rv;");
            w!(o, "    // Option");
            w!(o, "    if(size==0) {{");
            self.success(o);
            w!(o, "    }}");
            let f=self.item();
            let fty=f.typ().get_name();
            w!(o, "    MOL_CALL_SUBPARSER(item, {}, size)", fty);
            self.success(o);
            w!(o, "}}");
        }
        Ok(())
    }
}

impl GenReader for ast::Union {
    fn gen_reader_interfaces_internal<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        Err(std::io::Error::new(std::io::ErrorKind::Other, "Union is unimplemented"))
    }

    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        Err(std::io::Error::new(std::io::ErrorKind::Other, "Union is unimplemented"))
    }
}

impl GenReader for ast::Array {
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        if self.item().typ().is_byte() {
            w!(o, "struct {}_state {{ struct bytes_state state; }};", self.name());
            w!(o, "struct {}_callbacks {{ struct bytes_callbacks cb; }};", self.name());
            let i_macro_sig_tail = "_init_state(g, c, cbs)";
            let i_macro_content = "mol_bytes_init_state(&(g->state), c, &((cbs)->cb))";
            self.define_reader_macro(o, &i_macro_sig_tail, &i_macro_content)?;
            let macro_sig_tail = "_parse(g, c, cbs, sz)";
            let macro_content = format!(
                "mol_parse_bytes(&(g->state), c, &((cbs)->cb), {})", self.item_count());
            self.define_reader_macro(o, &macro_sig_tail, &macro_content)?;
        } else {
            w!(o, "struct {}_state {{ mol_num_t state_num }};", self.name());
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(o, "    void (*end)();");
            w!(o, "    struct item *{};", self.item().typ().get_name());
            w!(o, "}};");
            w!(o, "");
            self.start_init_function(o);
            w!(o, "        state->state_num=0; ");
            w!(o, "        MOL_INIT_SUBPARSER(item, {field_type});", field_type=self.item().typ().get_name());
            w!(o, "    }} ");
            self.start_parser_function(o);
            w!(o, "    if(size != {} && size != -1) return REJECT;", self.total_size());
            w!(o, "    while(state->state_num < {length}) {{ ", length=self.item_count());
            w!(o, "        MOL_CALL_SUBPARSER(item, {}, {})", self.item().typ().get_name(), self.item_size());
            w!(o, "        if(rv.complete) {{ ");
            w!(o, "            MOL_INIT_SUBPARSER(item, {field_type});", field_type=self.item().typ().get_name());
            w!(o, "            state->state_num++; ");
            w!(o, "        }} ");
            w!(o, "    }}");
            self.success(o);
            w!(o, "    return INCOMPLETE;");
            w!(o, "}}");
        }
        Ok(())
    }
}

impl GenReader for ast::Struct {
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            w!(o, "struct {}_state {{ mol_num_t state_num; }};", self.name());
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(o, "    void (*end)();");
            for (f, field_size) in self.fields().iter().zip(self.field_sizes().iter()) {
                w!(o, "    struct {}_callbacks *{};", f.typ().get_name(), f.name());
            }
            w!(o, "}};");
            w!(o, "");
        }
        {
            self.start_init_function(o);
            w!(o, "    s->state_num=0;");
            match self.fields().first() {
                None => { }
                Some(f) => w!(o, "    MOL_INIT_SUBPARSER({field_name}, {field_type});", field_name=f.name(), field_type=f.typ().get_name())
            }
            w!(o, "}}");
            w!(o, "");
            self.start_parser_function(o);
            w!(o, "    // Struct");
            w!(o, "    switch(s->state_num) {{");
            for (i, ((f, nf), fsz)) in self.fields().iter().zip(self.fields().iter().skip(1)).zip(self.field_sizes().iter()).enumerate() {
                let fty = f.typ().get_name();
                let nfty = nf.typ().get_name();
                w!(o, "        case {}: {{", i);
                w!(o, "            MOL_CALL_SUBPARSER({field_name}, {field_type}, {field_size})", field_name=f.name(), field_type=fty, field_size=fsz);
                w!(o, "            s->state_num++;");
                w!(o, "            MOL_INIT_SUBPARSER({field_name}, {field_type})", field_name=nf.name(), field_type=nfty);
                w!(o, "        }}");
            }
            match self.fields().iter().zip(self.field_sizes().iter()).last() {
              None => { Err(std::io::Error::new(std::io::ErrorKind::Other, "Empty table is unimplemented")) }
              Some((lf, lfsz)) => {
                let lfty=lf.typ().get_name();
                w!(o, "        case {}: {{", self.fields().len()-1);
                w!(o, "            MOL_CALL_SUBPARSER({field_name}, {field_type}, {field_size})", field_name=lf.name(), field_type=lfty, field_size=lfsz);
                w!(o, "        }}");
                w!(o, "    }}");
                self.success(o);
                w!(o, "}}");
                w!(o, "");
                Ok(())
              }
            }
        }
    }
}

impl GenReader for ast::FixVec {
    fn gen_reader_function_verify<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        {
            w!(writer, "struct {}_state {{ mol_num_t state_num; mol_num_t length; }};", self.name());
            w!(writer, "struct {}_callbacks {{", self.name());
            w!(writer, "    void (*start)();");
            w!(writer, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(writer, "    void (*end)();");
            if ! self.item().typ().is_byte() {
                w!(writer, "    struct {}_callbacks *item;", self.item().typ().get_name());
            } else {
                w!(writer, "    void (*body_chunk)(uint8_t*, mol_num_t);");
            }
            w!(writer, "}};");
            w!(writer, "");
            self.start_init_function(writer);
            w!(writer, "    s->state_num=0;");
            w!(writer, "    MOL_INIT_NUM();");
            w!(writer, "}}");

            self.start_parser_function(writer);
            w!(writer, "    if(s->state_num == 0) {{");
            w!(writer, "        MOL_CALL_NUM(s->length);");
            w!(writer, "        s->state_num++;");
            w!(writer, "        if(size != -1 && (s->length * {} + 4) != size) return REJECT;", self.item_size());
            if ! self.item().typ().is_byte() {
                w!(writer, "        MOL_INIT_SUBPARSER(item, {});", self.item().typ().get_name());
            }
            w!(writer, "    }}");
            if self.item().typ().is_byte() {
                w!(writer, "    if(s->state_num-1 < s->length) {{");
                w!(writer, "        mol_num_t needed=s->length+1-s->state_num;");
                w!(writer, "        mol_num_t available=chunk->length-chunk->consumed;");
                w!(writer, "        mol_num_t copy=needed<available?needed:available;");
                w!(writer, "        if(cb && cb->body_chunk) cb->body_chunk(chunk->ptr + chunk->consumed, copy);");
                w!(writer, "        chunk->consumed+=copy;");
                w!(writer, "        s->state_num+=copy;");
                w!(writer, "        if(s->state_num-1 < s->length) return INCOMPLETE;");
                w!(writer, "        DONE();");
                w!(writer, "    }}");
            } else {
                w!(writer, "    while(s->state_num-1 < s->length) {{");
                w!(writer, "        MOL_CALL_SUBPARSER(item, {}, {});", self.item().typ().get_name(), self.item_size());
                w!(writer, "        MOL_INIT_SUBPARSER(item, {});", self.item().typ().get_name());
                w!(writer, "        s->state_num++;");
                w!(writer, "    }}");
                w!(writer, "    DONE();");
            }
            w!(writer, "}}");
        }
        Ok(())
    }
}

impl GenReader for ast::DynVec {
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        // Only supporting DynVec of Table and DynVec of FixVec
        // Start without length validation.
        {
            w!(o, "struct {}_state {{", self.name());
            w!(o, "    mol_num_t state_num;");
            w!(o, "    mol_num_t field_idx;");
            w!(o, "    mol_num_t total_size;");
            w!(o, "    mol_num_t first_offset;");
            w!(o, "}};");
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(o, "    void (*end)();");
            w!(o, "    struct {}_callbacks *item;", self.item().typ().get_name());
            w!(o, "}};");
            w!(o, "");
        }
        {
            self.start_init_function(o);
            w!(o, "    s->state_num=0;");
            w!(o, "    s->field_idx=0;");
            w!(o, "    MOL_INIT_NUM();");
            w!(o, "}}");
            w!(o, "");
            self.start_parser_function(o);
            w!(o, "    // DynVec");
            w!(o, "    mol_rv rv;");
            w!(o, "    switch(s->state_num) {{");
            w!(o, "        case 0:");
            w!(o, "            MOL_CALL_NUM(s->total_size);");
            w!(o, "            MOL_INIT_NUM();");
            w!(o, "            if(s->total_size==4) {{");
                                   self.success(o);
            w!(o, "            }}");
            w!(o, "            s->state_num++;");
            w!(o, "        case 1:");
            w!(o, "            MOL_CALL_NUM(s->first_offset);");
            w!(o, "            MOL_INIT_NUM();");
            w!(o, "            s->field_idx=1;");
            w!(o, "            s->state_num++;");
            w!(o, "        case 2:");
            w!(o, "            while(s->field_idx < (s->first_offset>>2)-1) {{ ");
            w!(o, "                mol_num_t scratch;");
            w!(o, "                MOL_CALL_NUM(scratch);");
            w!(o, "                MOL_INIT_NUM();");
            w!(o, "                s->field_idx++;");
            // TODO: ensure equality of item-internal and item-external sizes.
            w!(o, "            }}");
            let fty=self.item().typ().get_name();
            w!(o, "            MOL_INIT_SUBPARSER(item, {field_type})", field_type=fty);
            w!(o, "            s->field_idx=0;");
            w!(o, "            s->state_num++;");
            w!(o, "        case 3:");
            w!(o, "            while(s->field_idx < (s->first_offset>>2)-1) {{ ");
            w!(o, "                MOL_CALL_SUBPARSER(item, {field_type}, -1)", field_type=fty);
            w!(o, "                MOL_INIT_SUBPARSER(item, {field_type})", field_type=fty);
            w!(o, "                s->field_idx++;");
            w!(o, "            }}");
            w!(o, "    }}");
            self.success(o);
            w!(o, "}}");
        }
        Ok(())
    }
}

impl GenReader for ast::Table {
    fn gen_reader_function_verify<W: io::Write>(&self, o: &mut W) -> io::Result<()> {
        {
            w!(o, "struct {}_state {{", self.name());
            w!(o, "    mol_num_t state_num;");
            w!(o, "    mol_num_t field_idx;");
            w!(o, "    mol_num_t total_size;");
            for f in self.fields().iter() {
                w!(o, "    mol_num_t {fn}_offset;", fn=f.name());
            }
            w!(o, "}};");
            w!(o, "struct {}_callbacks {{", self.name());
            w!(o, "    void (*start)();");
            w!(o, "    void (*chunk)(uint8_t*, mol_num_t);");
            w!(o, "    void (*end)();");
            for f in self.fields().iter() {
                w!(o, "    struct {}_callbacks *{};", f.typ().get_name(), f.name());
            }
            w!(o, "}};");
            w!(o, "");
        }
        {
            self.start_init_function(o);
            w!(o, "    s->state_num=0;");
            w!(o, "    s->field_idx=0;");
            w!(o, "    MOL_INIT_NUM();");
            w!(o, "}}");
            w!(o, "");
            self.start_parser_function(o);
            w!(o, "    // Table");
            w!(o, "    mol_rv rv;");
            w!(o, "    switch(s->state_num) {{");
            w!(o, "        case 0:");
            w!(o, "            MOL_CALL_NUM(s->total_size);");
            w!(o, "            MOL_INIT_NUM();");
            w!(o, "            if(size!=-1 && s->total_size != size) return REJECT;");
            w!(o, "            s->state_num++;");
            w!(o, "        case 1:");
            w!(o, "            switch(s->field_idx) {{");
            for (i, f) in self.fields().iter().enumerate() {
                w!(o, "                case {}: {{", i);
                w!(o, "                    MOL_CALL_NUM(s->{}_offset);", f.name());
                w!(o, "                    MOL_INIT_NUM();");
                w!(o, "                    s->field_idx++;");
                w!(o, "                }}");
            }
            w!(o, "            }}");
            w!(o, "            s->state_num++;");
            w!(o, "            s->field_idx=0;");
            w!(o, "            MOL_INIT_SUBPARSER({}, {});", self.fields()[0].name(), self.fields()[0].typ().get_name()); // fails on empty table.
            w!(o, "        case 2:");
            w!(o, "            switch(s->field_idx) {{");
            for (i, (f, nf)) in self.fields().iter().zip(self.fields().iter().skip(1)).enumerate() {
                let fty = f.typ().get_name();
                let nfty = nf.typ().get_name();
                w!(o, "            case {}: {{", i);
                w!(o, "                MOL_CALL_SUBPARSER({}, {}, s->{}_offset - s->{}_offset);", f.name(), fty, nf.name(), f.name());
                w!(o, "                MOL_INIT_SUBPARSER({}, {});", nf.name(), nfty);
                w!(o, "                s->field_idx++;");
                w!(o, "            }}");
            }
            match self.fields().last() {
              None => { Err(std::io::Error::new(std::io::ErrorKind::Other, "Empty table is unimplemented")) }
              Some(lf) => {
                let lfty=lf.typ().get_name();
                w!(o, "            case {}: {{", self.fields().len()-1);
                w!(o, "                MOL_CALL_SUBPARSER({}, {}, s->total_size-s->{}_offset);", lf.name(), lfty, lf.name());
                w!(o, "                s->state_num++;");
                w!(o, "            }}");
                w!(o, "        }}");
                w!(o, "    }}");
                self.success(o);
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
}
