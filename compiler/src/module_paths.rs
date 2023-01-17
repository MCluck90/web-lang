pub fn from_package_import(scope: &String, package: &String, path: &Vec<String>) -> String {
    let inner_path = path.join("/");
    if inner_path.is_empty() {
        format!("./{}/{}.nux", scope, package)
    } else {
        format!("./{}/{}/{}.nux", scope, package, inner_path)
    }
    .to_string()
}
