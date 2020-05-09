#![allow(dead_code)]

use convert_case::{Case, Casing};
use error_rules::*;
use std::io::Write;

#[derive(Debug, Error)]
enum Error {
    #[error_from]
    Reqwest(reqwest::Error),
    #[error_from]
    Json(json::Error),
    #[error_from]
    Io(std::io::Error),
}

enum PlotlyType {
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

struct RustType {
    plotly_type: PlotlyType,
    subtype: Option<String>,
}

impl RustType {
    fn new(s: &str, subtype: Option<String>) -> Self {
        let plotly_type = match s {
            "angle" => PlotlyType::Angle,
            "any" => PlotlyType::Any,
            "boolean" => PlotlyType::Boolean,
            "color" => PlotlyType::Color,
            "colorlist" => PlotlyType::Colorlist,
            "colorscale" => PlotlyType::ColorScale,
            "data_array" => PlotlyType::DataArray,
            "enumerated" => PlotlyType::Enumerated,
            "flaglist" => PlotlyType::Flaglist,
            "info_array" => PlotlyType::InfoArray,
            "integer" => PlotlyType::Integer,
            "number" => PlotlyType::Number,
            "string" => PlotlyType::String,
            "subplotid" => PlotlyType::SubplotId,
            _ => panic!("unsupported type: {}", s),
        };

        Self {
            plotly_type,
            subtype,
        }
    }

    pub fn num_lifetimes(&self) -> usize {
        match self.plotly_type {
            // strings
            PlotlyType::Color | PlotlyType::String | PlotlyType::SubplotId => 1,
            PlotlyType::Flaglist => 1,
            PlotlyType::Colorlist => 1,
            PlotlyType::ColorScale => 1,
            PlotlyType::DataArray => {
                if let Some(subtype) = &self.subtype {
                    // TODO: this is a hack
                    if subtype.starts_with("&") {
                        2
                    } else {
                        1
                    }
                } else {
                    1
                }
            }
            PlotlyType::InfoArray => 1,
            _ => 0,
        }
    }

    pub fn num_generics(&self) -> usize {
        match self.plotly_type {
            PlotlyType::DataArray => {
                if self.subtype.is_some() {
                    0
                } else {
                    1
                }
            }
            _ => 0,
        }
    }

    pub fn to_str(&self, lifetimes: Option<&[String]>, generics: Option<&[String]>) -> String {
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

        match self.plotly_type {
            // strings
            PlotlyType::Color | PlotlyType::String | PlotlyType::SubplotId => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "&{} str", lifetimes[0]).unwrap();
            }
            PlotlyType::Enumerated => {
                write!(&mut v, "{}", self.subtype.as_ref().unwrap()).unwrap();
            }
            PlotlyType::Flaglist => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "crate::Flaglist<{}>", lifetimes[0]).unwrap();
            }
            PlotlyType::Colorlist => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "&{} [&{} str]", lifetimes[0], lifetimes[0]).unwrap();
            }
            PlotlyType::Angle => {
                write!(&mut v, "crate::Angle").unwrap();
            }
            PlotlyType::Any => {
                write!(&mut v, "crate::Any").unwrap();
            }
            PlotlyType::Boolean => write!(&mut v, "bool").unwrap(),
            PlotlyType::ColorScale => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "crate::ColorScale<{}>", lifetimes[0]).unwrap();
            }
            PlotlyType::DataArray => {
                let lifetimes = lifetimes.unwrap();
                let generics = generics.unwrap();
                let elemtype = if let Some(subtype) = &self.subtype {
                    subtype
                } else {
                    &generics[0]
                };
                write!(&mut v, "&{} [{}]", lifetimes[0], elemtype).unwrap();
            }
            PlotlyType::InfoArray => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "&{} crate::InfoArray", lifetimes[0]).unwrap();
            }
            PlotlyType::Integer => {
                write!(&mut v, "u64").unwrap();
            }
            PlotlyType::Number => {
                write!(&mut v, "f64").unwrap();
            }
        }

        String::from_utf8(v).unwrap()
    }
}

fn make_lt_and_g(
    attrname: &str,
    num_lifetimes: usize,
    num_generics: usize,
) -> (Vec<String>, Vec<String>) {
    let mut type_lifetimes = Vec::with_capacity(num_lifetimes);
    let mut type_generics = Vec::with_capacity(num_generics);

    for _ in 0..type_lifetimes.capacity() {
        type_lifetimes.push("'a".to_string());
    }

    for i in 0..type_generics.capacity() {
        type_generics.push(format!("{}G{}", attrname.to_case(Case::UpperFlat), i));
    }

    (type_lifetimes, type_generics)
}

fn get_dataarray_type(attrname: &str) -> Option<String> {
    match attrname {
        "ticktext" => Some("&'a str".to_string()),
        "categoryarray" => Some("usize".to_string()),
        "ids" => Some("&'a str".to_string()),
        _ => None,
    }
}

fn gen_struct<F: std::io::Write>(
    f: &mut F,
    modname: Option<&str>,
    structname: &str,
    description: Option<&str>,
    attrs: &json::JsonValue,
) -> Result<(usize, usize), Error> {
    let mut code: Vec<u8> = Vec::new();
    let mut modcode: Vec<u8> = Vec::new();
    let mut fields: Vec<u8> = Vec::new();
    let mut lifetimes: Vec<String> = Vec::new();
    let mut generics: Vec<String> = Vec::new();

    let namespace = if let Some(modname) = &modname {
        format!("{}::", modname)
    } else {
        "".to_string()
    };

    for (attrname_js, attr) in attrs.entries() {
        if attrname_js == "box" {
            continue;
        }

        match attrname_js {
            "customdata" | "type" | "_deprecated" | "editType" | "description" | "impliedEdits"
            | "_isSubplotObj" | "_arrayAttrRegexps" => continue,
            "role" => {
                assert_eq!(attr.as_str().unwrap(), "object");
                break;
            }
            _ => (),
        }
        let attrname = attrname_js.to_case(Case::Snake);

        if let Some(description) = attr["description"].as_str() {
            if description.contains("deprecated!") {
                continue;
            }
        }

        if let Some(valtype) = attr["valType"].as_str() {
            let subtypename = match valtype {
                "enumerated" => {
                    let subtypename = attrname.to_case(Case::Pascal);

                    match subtypename.as_str() {
                        "Operation" | "Scaleanchor" => break,
                        _ => (),
                    }

                    writeln!(&mut modcode, "#[derive(Serialize)]")?;
                    writeln!(&mut modcode, "pub enum {} {{", subtypename)?;

                    for val in attr["values"].members() {
                        let mut handle_str = |s: &str| -> Result<(), Error> {
                            if s.len() == 0 {
                                return Ok(());
                            }
                            let s = match s {
                                "e" => "SmallE",
                                "E" => "BigE",
                                "<=" => "LE",
                                ">=" => "GE",
                                "=" => "EQ",
                                ">" => "GT",
                                "<" => "LT",
                                _ => s,
                            };

                            let s = s.replace("+", "And");
                            let s = s.replace("|", "Or");
                            let s = s.replace("/", "Slash");
                            let s = s.replace("\\", "Backslash");

                            writeln!(&mut modcode, "    #[serde(rename = \"{}\")]", s)?;
                            writeln!(&mut modcode, "    {},", s.to_case(Case::Pascal))?;

                            Ok(())
                        };
                        match val {
                            json::JsonValue::Short(s) => handle_str(s)?,
                            json::JsonValue::String(s) => handle_str(s)?,
                            _ => (),
                        }
                    }

                    writeln!(&mut modcode, "}}")?;

                    Some(format!("{}{}", namespace, subtypename))
                }
                "data_array" => get_dataarray_type(&attrname),
                _ => None,
            };

            let rusttype = RustType::new(valtype, subtypename);
            let (type_lifetimes, type_generics) =
                make_lt_and_g(&attrname, rusttype.num_lifetimes(), rusttype.num_generics());

            let typestr = rusttype.to_str(Some(&type_lifetimes), Some(&type_generics));

            writeln!(&mut fields, "    #[serde(rename = \"{}\")]", attrname_js)?;
            writeln!(
                &mut fields,
                "    #[serde(skip_serializing_if = \"Option::is_none\")]"
            )?;
            writeln!(&mut fields, "    {}: Option<{}>,", attrname, typestr)?;

            if type_lifetimes.len() > 0 && lifetimes.len() == 0 {
                lifetimes.push("'a".to_string());
            }
            generics.extend_from_slice(&type_generics);

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
        } else if let Some(role) = attr["role"].as_str() {
            if role != "object" {
                panic!("role {} is not supported", role);
            }
            if attr.has_key("items") {
                assert_eq!(attr["items"].len(), 1);
                // TODO: array support
                continue;
            }

            let substructname = attrname.to_case(Case::Pascal);

            let (ss_num_lifetimes, ss_num_generics) = gen_struct(
                &mut modcode,
                Some(&attrname),
                &substructname,
                attr["description"].as_str(),
                attr,
            )?;
            let (type_lifetimes, type_generics) =
                make_lt_and_g(&attrname, ss_num_lifetimes, ss_num_generics);

            let params_joined = [&type_lifetimes[..], &type_generics[..]]
                .concat()
                .join(", ");
            let typestr = format!("{}{}<{}>", namespace, substructname, params_joined);

            writeln!(&mut fields, "    #[serde(rename = \"{}\")]", attrname_js)?;
            writeln!(
                &mut fields,
                "    #[serde(skip_serializing_if = \"crate::IsEmpty::is_empty\")]"
            )?;
            writeln!(
                &mut fields,
                "    {}: crate::IsEmpty<{}>,",
                attrname, typestr
            )?;

            if type_lifetimes.len() > 0 && lifetimes.len() == 0 {
                lifetimes.push("'a".to_string());
            }
            generics.extend_from_slice(&type_generics);

            if let Some(description) = attr["description"].as_str() {
                writeln!(&mut code, "    /// {}", description)?;
            }
            writeln!(
                &mut code,
                "    pub fn {}(&mut self) -> &mut {} {{",
                attrname, typestr
            )?;
            writeln!(&mut code, "        self.{}.is_empty = false;", attrname)?;
            writeln!(&mut code, "        &mut self.{}.data", attrname)?;
            writeln!(&mut code, "    }}")?;
        } else {
            panic!(
                "unsupported member: {}/{} = {:?}",
                structname, attrname_js, attr
            );
        }
    }

    let params_joined = [&lifetimes[..], &generics[..]].concat().join(", ");

    if modname.is_none() {
        writeln!(f, "use serde::Serialize;")?;
    }
    writeln!(f)?;
    if let Some(description) = description {
        writeln!(f, "/// {}", description)?;
    }
    writeln!(f, "#[derive(Default, Serialize)]")?;
    writeln!(f, "pub struct {}<{}> {{", structname, params_joined)?;
    f.write(&fields)?;
    writeln!(f, "}}")?;
    writeln!(f)?;

    writeln!(
        f,
        "impl<{}> {}<{}> {{",
        params_joined, structname, params_joined
    )?;
    f.write(&code)?;
    writeln!(f, "}}")?;

    if modcode.len() > 0 {
        if let Some(modname) = &modname {
            writeln!(f, "pub mod {} {{", modname)?;
            writeln!(f, "use serde::Serialize;")?;
        }

        f.write(&modcode)?;

        if modname.is_some() {
            writeln!(f, "}}")?;
        }
    }

    Ok((lifetimes.len(), generics.len()))
}

fn main() -> Result<(), Error> {
    let pkg_version = env!("CARGO_PKG_VERSION");
    let plotly_version = pkg_version.splitn(2, "-").next().unwrap();
    let schema_url = format!(
        "https://raw.githubusercontent.com/plotly/plotly.js/v{}/dist/plot-schema.json",
        plotly_version
    );

    let local_schema_path = std::path::Path::new("plot-schema.json");
    let schema = if local_schema_path.exists() {
        std::fs::read_to_string(local_schema_path)?
    } else {
        reqwest::blocking::get(&schema_url)?.text()?
    };
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
    for (mut tracename, trace) in schema["traces"].entries() {
        if tracename == "box" {
            tracename = "box_";
        }
        let structname = tracename.to_case(Case::Pascal);

        writeln!(&mut f_traces_mod, "pub mod {};", tracename)?;

        let mut f = std::fs::File::create(traces_path.join(format!("{}.rs", tracename)))?;
        gen_struct(
            &mut f,
            None,
            &structname,
            trace["meta"]["description"].as_str(),
            &trace["attributes"],
        )?;
    }

    let mut f_transforms_mod = std::fs::File::create(transforms_path.join("mod.rs"))?;
    for (transformname, transform) in schema["transforms"].entries() {
        let structname = transformname.to_case(Case::Pascal);

        writeln!(&mut f_transforms_mod, "pub mod {};", transformname)?;

        let mut f = std::fs::File::create(transforms_path.join(format!("{}.rs", transformname)))?;
        gen_struct(&mut f, None, &structname, None, &transform["attributes"])?;
    }

    let mut f = std::fs::File::create(out_path.join("config.rs"))?;
    gen_struct(&mut f, None, "Config", None, &schema["config"])?;

    let mut f = std::fs::File::create(out_path.join("layout.rs"))?;
    gen_struct(
        &mut f,
        None,
        "Layout",
        None,
        &schema["layout"]["layoutAttributes"],
    )?;

    let mut f = std::fs::File::create(out_path.join("animation.rs"))?;
    gen_struct(&mut f, None, "Animation", None, &schema["animation"])?;

    let mut f_mod = std::fs::File::create(out_path.join("mod.rs"))?;
    writeln!(&mut f_mod, "pub mod traces;")?;
    writeln!(&mut f_mod, "pub mod transforms;")?;
    writeln!(&mut f_mod, "pub mod layout;")?;
    writeln!(&mut f_mod, "pub mod config;")?;
    writeln!(&mut f_mod, "pub mod animation;")?;

    println!("cargo:rerun-if-changed=build.rs");

    Ok(())
}
