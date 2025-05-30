use std::fs;
use std::path::PathBuf;

use clap::Parser;
use clap::Subcommand;
use lox_rs::Lexer;
use miette::IntoDiagnostic;
use miette::WrapErr;

#[derive(Debug, Parser)]
#[command()]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    Tokenize { src: PathBuf },
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Tokenize { src } => {
            let file_contents = fs::read_to_string(src)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading {} failed", src.display()))?;

            for token in Lexer::new(&file_contents) {
                let token = token?;
                println!("{token}");
            }
            println!("EOF  null");
        }
    }
    Ok(())
}
