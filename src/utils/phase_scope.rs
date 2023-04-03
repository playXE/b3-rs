use std::{sync::Mutex, collections::HashMap};

use once_cell::sync::Lazy;
static SCOPE_INFO: Lazy<Mutex<HashMap<String, ScopeInfo>>> = Lazy::new(|| Mutex::new(HashMap::new()));

struct ScopeInfo {
    count: usize,
    time: u64,
}

pub fn phase_scope<R>(name: &str, f: impl FnOnce() -> R) -> R {
    //let start = std::time::Instant::now();
    let r = f();
    //let elapsed = start.elapsed().as_micros();
    //let mut info = SCOPE_INFO.lock().unwrap();
    //let entry = info.entry(name.to_string()).or_insert(ScopeInfo { count: 0, time: 0 });
    //entry.count += 1;
    //entry.time += elapsed as u64;

    r
}

pub fn print_scope_info() {
    let info = SCOPE_INFO.lock().unwrap();
    for (name, info) in info.iter() {
        println!("{}: {} times, {:.2} ms", name, info.count, info.time as f64 / info.count as f64 / 1000.0);
    }
}