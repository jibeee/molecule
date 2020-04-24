use molecule_codegen::{IntermediateFormat, Language};

fn main() {
    moleculec::build_commandline(Language::CStreaming, IntermediateFormat::JSON).execute();
}
