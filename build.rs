fn execute(command: &str, args: &[&str]) -> Result<(), Box<dyn std::error::Error>> {
    let output = std::process::Command::new(command).args(args).output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(stderr.into());
    }

    Ok(())
}

fn move_file(from: &str, to: &str) -> std::io::Result<()> {
    std::fs::copy(from, to)?;
    std::fs::remove_file(from)?;

    Ok(())
}

fn main() {
    println!("cargo:rerun-if-changed=src/air/opcode.opcodes");
    println!("cargo:rerun-if-changed=src/air/opcode_generator.rb");
    let out_dir = std::env::var("OUT_DIR").unwrap();

    execute(
        "ruby",
        &["src/air/opcode_generator.rb", "src/air/opcode.opcodes"],
    )
    .unwrap();
    move_file(
        "opcode_generated.rs",
        &format!("{}/opcode_generated.rs", out_dir),
    )
    .unwrap();
    move_file("opcode_utils.rs", &format!("{}/opcode_utils.rs", out_dir)).unwrap();
    move_file("opcode.rs", &format!("{}/opcode.rs", out_dir)).unwrap();
}
