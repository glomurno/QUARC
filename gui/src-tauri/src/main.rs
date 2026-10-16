// Prevents an extra console window on Windows in release builds.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

fn main() {
    tauri::Builder::default()
        // The shell plugin lets the frontend spawn the `quarc serve` sidecar and
        // talk to it over stdio. All request logic lives in the JS-side rpc.ts.
        .plugin(tauri_plugin_shell::init())
        // The dialog plugin backs the native "choose file" picker (rpc.pickQuarcFile).
        .plugin(tauri_plugin_dialog::init())
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
