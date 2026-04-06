use serde::Deserialize;

#[derive(Deserialize, Default, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    pub check_for_updates: bool,
}
