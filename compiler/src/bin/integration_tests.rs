use prettydiff::diff_chars;
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::process::Command;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Snapshot {
    stdout: String,
    stderr: String,
    exit_code: i32,
}

enum Mode {
    Run,
    Update,
}

fn main() {
    let flag = std::env::args().nth(1).unwrap_or("--run".into());
    println!("{}", flag);
    let mode = match flag.as_str() {
        "--run" => Mode::Run,
        "--update" => Mode::Update,
        _ => panic!("Unrecognized flag: {}", flag),
    };
    let examples_dir = Path::new("examples");
    let example_files = examples_dir
        .read_dir()
        .unwrap()
        .map(|f| f.unwrap().path())
        .filter(|f| f.extension().map(|p| p.to_str().unwrap()).unwrap_or("") != "snapshot");

    for file in example_files {
        run_test(&file, &mode);
    }
}

fn run_test(path_to_file: &Path, mode: &Mode) {
    let path_to_snapshot = format!("{}.snapshot", path_to_file.to_str().unwrap());
    let path_to_snapshot = Path::new(&path_to_snapshot);
    let maybe_old_snapshot = if path_to_snapshot.exists() {
        Some(
            serde_json::from_str::<Snapshot>(&std::fs::read_to_string(path_to_snapshot).unwrap())
                .unwrap(),
        )
    } else {
        None
    };

    let cargo = Command::new("cargo")
        .args(["run", path_to_file.to_str().unwrap()])
        .output()
        .unwrap();

    let mut stdout = String::from_utf8(cargo.stdout).unwrap();
    let mut stderr = String::from_utf8(cargo.stderr).unwrap();
    let mut exit_code = cargo.status.code().unwrap();
    if cargo.status.success() {
        std::fs::write("target/_integration-test.js", stdout)
            .expect("failed to write to test file");

        let node = Command::new("node")
            .args(["target/_integration-test.js"])
            .output()
            .unwrap();

        stdout = String::from_utf8(node.stdout).unwrap();
        stderr = String::from_utf8(node.stderr).unwrap();
        exit_code = node.status.code().unwrap();

        std::fs::remove_file("target/_integration-test.js").expect("failed to delete test file");
    }

    let new_snapshot = Snapshot {
        stdout,
        stderr,
        exit_code,
    };

    let serialized = serde_json::to_string(&new_snapshot).unwrap();
    match (mode, maybe_old_snapshot) {
        (Mode::Run, None) => {
            println!(
                "⚠ {}: No snapshot. Run with the --update flag to save a snapshot.\nstdout: {}\nstderr: {}\nexit code: {}",
                path_to_file.to_str().unwrap(),
                new_snapshot.stdout,
                new_snapshot.stderr,
                new_snapshot.exit_code
            );
        }
        (Mode::Run, Some(old_snapshot)) => {
            let stdout_diff = diff_chars(&old_snapshot.stdout, &new_snapshot.stdout).format();
            let stderr_diff = diff_chars(&old_snapshot.stderr, &new_snapshot.stderr).format();

            if old_snapshot.stdout != new_snapshot.stdout
                || old_snapshot.stderr != new_snapshot.stderr
                || new_snapshot.exit_code != old_snapshot.exit_code
            {
                println!("❌ {}", path_to_file.to_str().unwrap());
                if old_snapshot.stdout != new_snapshot.stdout {
                    eprintln!("stdout:\n{}", stdout_diff);
                }

                if old_snapshot.stderr != new_snapshot.stderr {
                    eprintln!("stderr:\n{}", stderr_diff);
                }

                if new_snapshot.exit_code != old_snapshot.exit_code {
                    eprintln!(
                        "Expected exit code of {} but received {}",
                        old_snapshot.exit_code, new_snapshot.exit_code
                    );
                }
            } else {
                println!("✅ {}", path_to_file.to_str().unwrap());
            }
        }
        (Mode::Update, _) => {
            std::fs::write(path_to_snapshot, serialized).expect("failed to save snapshot");
            println!("✅ {}: Saved snapshot!", path_to_file.to_str().unwrap());
        }
    }

    // TODO: Pull up and compare against snapshot in Run mode
    // let path_to_snapshot = format!("{}.snapshot", path_to_file.to_str().unwrap());
    // let maybe_snapshot = Path::new(&path_to_snapshot);
}
