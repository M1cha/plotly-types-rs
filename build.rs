#![allow(dead_code)]

use convert_case::{Case, Casing};
use error_rules::*;
use std::io::Write;

const PLOTLY_SCHEMA_URL: &str = concat!(
    "https://raw.githubusercontent.com/plotly/plotly.js/v",
    env!("CARGO_PKG_VERSION"),
    "/dist/plot-schema.json"
);

#[derive(Debug, Error)]
enum Error {
    #[error_from]
    Reqwest(reqwest::Error),
    #[error_from]
    Json(json::Error),
    #[error_from]
    Io(std::io::Error),
}

enum RustType {
    Angle,
    Any,
    Boolean,
    Color,
    Colorlist,
    ColorScale,
    DataArray,
    Enumerated,
    Flaglist,
    InfoArray,
    Integer,
    Number,
    String,
    SubplotId,
}

impl From<&str> for RustType {
    fn from(s: &str) -> Self {
        match s {
            "angle" => Self::Angle,
            "any" => Self::Any,
            "boolean" => Self::Boolean,
            "color" => Self::Color,
            "colorlist" => Self::Colorlist,
            "colorscale" => Self::ColorScale,
            "data_array" => Self::DataArray,
            "enumerated" => Self::Enumerated,
            "flaglist" => Self::Flaglist,
            "info_array" => Self::InfoArray,
            "integer" => Self::Integer,
            "number" => Self::Number,
            "string" => Self::String,
            "subplotid" => Self::SubplotId,
            _ => panic!("unsupported type: {}", s),
        }
    }
}

impl RustType {
    pub fn num_lifetimes(&self) -> usize {
        match self {
            // strings
            Self::Color | Self::String | Self::SubplotId => 1,
            Self::Flaglist => 1,
            Self::Colorlist => 1,
            Self::ColorScale => 2,
            Self::DataArray => 1,
            Self::InfoArray => 1,
            _ => 0,
        }
    }

    pub fn num_generics(&self) -> usize {
        match self {
            Self::DataArray => 1,
            _ => 0,
        }
    }

    pub fn to_str(
        &self,
        subtype: &str,
        lifetimes: Option<&[String]>,
        generics: Option<&[String]>,
    ) -> String {
        let mut v: Vec<u8> = Vec::new();

        let num_lifetimes = self.num_lifetimes();
        let num_generics = self.num_generics();

        if let Some(s) = &lifetimes {
            assert_eq!(num_lifetimes, s.len());
        } else {
            assert_eq!(num_lifetimes, 0);
        }

        if let Some(s) = &generics {
            assert_eq!(num_generics, s.len());
        } else {
            assert_eq!(num_generics, 0);
        }

        match self {
            // strings
            Self::Color | Self::String | Self::SubplotId => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "&{} str", lifetimes[0]).unwrap();
            }
            Self::Enumerated => {
                write!(&mut v, "{}", subtype).unwrap();
            }
            Self::Flaglist => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "&{} {}", lifetimes[0], subtype).unwrap();
            }
            Self::Colorlist => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "&{} [&{} str]", lifetimes[0], lifetimes[0]).unwrap();
            }
            Self::Angle => {
                write!(&mut v, "f64").unwrap();
            }
            Self::Any => {
                write!(&mut v, "crate::Any").unwrap();
            }
            Self::Boolean => write!(&mut v, "bool").unwrap(),
            Self::ColorScale => {
                let lifetimes = lifetimes.unwrap();
                write!(
                    &mut v,
                    "crate::ColorScale<{}, {}>",
                    lifetimes[0], lifetimes[1]
                )
                .unwrap();
            }
            Self::DataArray => {
                let lifetimes = lifetimes.unwrap();
                let generics = generics.unwrap();
                write!(&mut v, "&{} [{}]", lifetimes[0], generics[0]).unwrap();
            }
            Self::InfoArray => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "&{} {}", lifetimes[0], subtype).unwrap();
            }
            Self::Integer => {
                write!(&mut v, "u64").unwrap();
            }
            Self::Number => {
                write!(&mut v, "f64").unwrap();
            }
        }

        String::from_utf8(v).unwrap()
    }
}

fn gen_struct(
    path: &std::path::Path,
    structname: &str,
    description: Option<&str>,
    attrs: &json::JsonValue,
) -> Result<(), Error> {
    let mut code: Vec<u8> = Vec::new();
    let mut fields: Vec<u8> = Vec::new();
    let mut lifetimes: Vec<String> = Vec::new();
    let mut generics: Vec<String> = Vec::new();

    for (attrname_js, attr) in attrs.entries() {
        if attrname_js == "customdata" {
            continue;
        }
        let attrname = attrname_js.to_case(Case::Snake);

        if let Some(description) = attr["description"].as_str() {
            if description.contains("deprecated!") {
                continue;
            }
        }

        if let Some(valtype) = attr["valType"].as_str() {
            let rusttype = RustType::from(valtype);
            let mut type_lifetimes = Vec::with_capacity(rusttype.num_lifetimes());
            let mut type_generics = Vec::with_capacity(rusttype.num_generics());

            for i in 0..type_lifetimes.capacity() {
                type_lifetimes.push(format!("'{}_l{}", attrname, i));
            }

            for i in 0..type_generics.capacity() {
                type_generics.push(format!("{}G{}", attrname.to_case(Case::Upper), i));
            }

            let typestr = rusttype.to_str("usize", Some(&type_lifetimes), Some(&type_generics));

            writeln!(
                &mut fields,
                "    #[serde(skip_serializing_if = \"Option::is_none\")]"
            )?;
            writeln!(&mut fields, "    #[serde(rename = \"{}\")]", attrname_js)?;
            writeln!(&mut fields, "    {}: Option<{}>,", attrname, typestr)?;

            if let Some(description) = attr["description"].as_str() {
                writeln!(&mut code, "    /// {}", description)?;
            }
            writeln!(
                &mut code,
                "    pub fn {}(&mut self, {}: {}) -> &mut Self {{",
                attrname, attrname, typestr
            )?;
            writeln!(&mut code, "        self.{} = Some({});", attrname, attrname)?;
            writeln!(&mut code, "        self")?;
            writeln!(&mut code, "    }}")?;

            lifetimes.extend_from_slice(&type_lifetimes);
            generics.extend_from_slice(&type_generics);
        } else if let Some(role) = attr["role"].as_str() {
            if role != "object" {
                continue;
            }

            //writeln!(&mut fields, "    {}: {},", attrname, attrname.to_case(Case::Pascal))?;
        }
    }

    let params_joined = [&lifetimes[..], &generics[..]].concat().join(", ");

    let mut f = std::fs::File::create(path)?;
    writeln!(&mut f, "use serde::Serialize;")?;
    writeln!(&mut f)?;

    if let Some(description) = description {
        writeln!(&mut f, "/// {}", description)?;
    }
    writeln!(&mut f, "#[derive(Default, Serialize)]")?;
    writeln!(&mut f, "pub struct {}<{}> {{", structname, params_joined)?;
    f.write(&fields)?;
    writeln!(&mut f, "}}")?;
    writeln!(&mut f)?;

    writeln!(
        &mut f,
        "impl<{}> {}<{}> {{",
        params_joined, structname, params_joined
    )?;
    f.write(&code)?;
    writeln!(&mut f, "}}")?;

    Ok(())
}

fn main() -> Result<(), Error> {
    let schema = reqwest::blocking::get(PLOTLY_SCHEMA_URL)?.text()?;
    let schema = json::parse(&schema)?;

    let out_path = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());

    let traces_path = out_path.join("traces");
    if traces_path.exists() {
        std::fs::remove_dir_all(&traces_path)?;
    }
    std::fs::create_dir(&traces_path)?;

    let transforms_path = out_path.join("transforms");
    if transforms_path.exists() {
        std::fs::remove_dir_all(&transforms_path)?;
    }
    std::fs::create_dir(&transforms_path)?;

    let mut f_traces_mod = std::fs::File::create(traces_path.join("mod.rs"))?;
    for (tracename, trace) in schema["traces"].entries() {
        if tracename == "box" {
            continue;
        }
        let structname = tracename.to_case(Case::Pascal);

        writeln!(&mut f_traces_mod, "pub mod {};", tracename)?;

        gen_struct(
            &traces_path.join(format!("{}.rs", tracename)),
            &structname,
            trace["meta"]["description"].as_str(),
            &trace["attributes"],
        )?;
    }

    let mut f_transforms_mod = std::fs::File::create(transforms_path.join("mod.rs"))?;
    for (transformname, transform) in schema["transforms"].entries() {
        let structname = transformname.to_case(Case::Pascal);

        writeln!(&mut f_transforms_mod, "pub mod {};", transformname)?;

        gen_struct(
            &transforms_path.join(format!("{}.rs", transformname)),
            &structname,
            None,
            &transform["attributes"],
        )?;
    }

    gen_struct(
        &out_path.join("config.rs"),
        "Config",
        None,
        &schema["config"],
    )?;

    gen_struct(
        &out_path.join("layout.rs"),
        "Layout",
        None,
        &schema["layout"]["layoutAttributes"],
    )?;

    gen_struct(
        &out_path.join("animation.rs"),
        "Animation",
        None,
        &schema["animation"],
    )?;

    let mut f_mod = std::fs::File::create(out_path.join("mod.rs"))?;
    writeln!(&mut f_mod, "pub mod traces;")?;
    writeln!(&mut f_mod, "pub mod transforms;")?;
    writeln!(&mut f_mod, "pub mod layout;")?;
    writeln!(&mut f_mod, "pub mod config;")?;
    writeln!(&mut f_mod, "pub mod animation;")?;

    Ok(())
}
