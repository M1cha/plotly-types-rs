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
            PlotlyType::Colorlist => 1,
            PlotlyType::ColorScale => 1,
            PlotlyType::DataArray => {
                if let Some(subtype) = &self.subtype {
                    // TODO: this is a hack
                    if subtype.starts_with('&') {
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
        0
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
                lifetimes.get(0).unwrap();
                v.write_all(b"{STR}").unwrap();
            }
            PlotlyType::Enumerated => {
                write!(&mut v, "{}", self.subtype.as_ref().unwrap()).unwrap();
            }
            PlotlyType::Flaglist => {
                write!(&mut v, "{}", self.subtype.as_ref().unwrap()).unwrap();
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
                let elemtype = if let Some(subtype) = &self.subtype {
                    subtype
                } else {
                    "f64"
                };
                write!(&mut v, "&{} [{}]", lifetimes[0], elemtype).unwrap();
            }
            PlotlyType::InfoArray => {
                let lifetimes = lifetimes.unwrap();
                write!(&mut v, "&{} crate::InfoArray", lifetimes[0]).unwrap();
            }
            PlotlyType::Integer => {
                write!(&mut v, "i64").unwrap();
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

fn str2enum(s: &str) -> String {
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
    let s = s.replace("!", "Not");
    let s = s.replace("=", "Equal");
    let s = s.replace("/", "Slash");
    let s = s.replace("\\", "Backslash");
    let s = s.replace("^", "Caret");
    let s = s.replace("[", "OSqBr");
    let s = s.replace("]", "CSqBr");
    let s = s.replace("(", "OBr");
    let s = s.replace(")", "CBr");
    let s = s.replace("{", "OCrlBr");
    let s = s.replace("}", "CCrlBr");
    let s = s.replace("?", "QM");
    let s = s.replace("$", "Dollar");

    s.to_case(Case::Pascal)
}

fn escape_str(s: &str) -> String {
    s.replace("\\", "\\\\").replace("\"", "\\\"")
}

fn enum_impl_start<W: std::io::Write>(
    mut w: W,
    subtypename: &str,
    add_lifetime: bool,
) -> Result<(), Error> {
    let ltstr = if add_lifetime { "<'a>" } else { "" };
    writeln!(&mut w, "impl{} {}{} {{", ltstr, subtypename, ltstr)?;

    Ok(())
}

fn enum_impl_end<W: std::io::Write>(mut w: W) -> Result<(), Error> {
    writeln!(&mut w, "}}")?;

    Ok(())
}

fn impl_serialize_start<W: std::io::Write>(
    mut w: W,
    subtypename: &str,
    add_lifetime: bool,
) -> Result<(), Error> {
    let ltstr = if add_lifetime { "<'a>" } else { "" };
    writeln!(
        &mut w,
        "impl{} serde::Serialize for {}{} {{",
        ltstr, subtypename, ltstr
    )?;

    writeln!(&mut w, "    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {{")?;

    Ok(())
}

fn impl_serialize_end<W: std::io::Write>(mut w: W) -> Result<(), Error> {
    writeln!(&mut w, "    }}")?;
    writeln!(&mut w, "}}")?;

    Ok(())
}

fn gen_enum<W1, W2>(
    mut modcode: W1,
    mut serimpl: W2,
    values: &json::JsonValue,
) -> Result<usize, Error>
where
    W1: std::io::Write,
    W2: std::io::Write,
{
    let mut nmembers = 0;

    for val in values.members() {
        let mut handle_str = |namejs: &str| -> Result<Option<String>, Error> {
            if namejs.is_empty() || namejs.chars().nth(0).unwrap().is_ascii_digit() {
                return Ok(None);
            }
            let namerust = str2enum(namejs);

            writeln!(
                &mut serimpl,
                "            Self::{} => serializer.serialize_str(\"{}\"),",
                namerust,
                escape_str(namejs)
            )?;

            Ok(Some(namerust))
        };

        let namerust = match val {
            json::JsonValue::Short(v) => handle_str(v)?,
            json::JsonValue::String(v) => handle_str(v)?,
            json::JsonValue::Boolean(v) => {
                let namejs = if *v { "true" } else { "false" }.to_string();
                let namerust = if *v { "True" } else { "False" }.to_string();
                writeln!(
                    &mut serimpl,
                    "            Self::{} => serializer.serialize_bool({}),",
                    namerust, namejs
                )?;
                Some(namerust)
            }
            json::JsonValue::Number(n) => {
                let (positive, mantissa, exponent) = n.as_parts();
                assert_eq!(exponent, 0);

                let sign = if positive { "" } else { "Neg" };
                let namerust = format!("Num{}{}", sign, mantissa);
                writeln!(
                    &mut serimpl,
                    "            Self::{} => serializer.serialize_u64({}),",
                    namerust, mantissa
                )?;

                Some(namerust)
            }
            _ => panic!("unsupported enum vale {:?}", val),
        };

        if let Some(namerust) = namerust {
            writeln!(&mut modcode, "    {},", namerust)?;
            nmembers += 1;
        }
    }

    Ok(nmembers)
}

fn handle_enumerated<W>(
    attrname: &str,
    attr: &json::JsonValue,
    mut modcode: W,
) -> Result<String, Error>
where
    W: std::io::Write,
{
    let subtypename = attrname.to_case(Case::Pascal);
    let mut serimpl: Vec<u8> = Vec::new();

    impl_serialize_start(&mut serimpl, &subtypename, false)?;
    writeln!(&mut serimpl, "        match self {{")?;

    writeln!(&mut modcode, "#[derive(Clone, Debug)]")?;
    writeln!(&mut modcode, "pub enum {} {{", subtypename)?;
    gen_enum(&mut modcode, &mut serimpl, &attr["values"])?;
    writeln!(&mut modcode, "}}")?;

    writeln!(&mut serimpl, "        }}")?;
    impl_serialize_end(&mut serimpl)?;
    modcode.write_all(&serimpl)?;

    Ok(subtypename)
}

fn roundup(a: usize, b: usize) -> usize {
    (a + (b - 1)) & !(b - 1)
}

fn handle_flaglist<W>(
    attrname: &str,
    attr: &json::JsonValue,
    mut modcode: W,
) -> Result<String, Error>
where
    W: std::io::Write,
{
    let subtypename = attrname.to_case(Case::Pascal);
    let subtypeflagsname = format!("{}Flags", subtypename);
    let flags = match &attr["flags"] {
        json::JsonValue::Array(v) => v,
        _ => panic!("flags is not an array"),
    };
    let nbits = flags.len();
    let nbytes = roundup(nbits, 8) / 8;

    writeln!(&mut modcode, "#[derive(Default, Clone, Debug)]")?;
    writeln!(
        &mut modcode,
        "pub struct {} ([u8; {}]);",
        subtypeflagsname, nbytes
    )?;
    enum_impl_start(&mut modcode, &subtypeflagsname, false)?;

    let mut serimpl: Vec<u8> = Vec::new();
    impl_serialize_start(&mut serimpl, &subtypeflagsname, false)?;
    writeln!(&mut serimpl, "        #[allow(unused_mut)]")?;
    writeln!(&mut serimpl, "        let mut v: Vec<&str> = Vec::new();")?;
    for (bitabs, flag) in flags.iter().enumerate() {
        let namejs = flag.as_str().unwrap();
        let byte = bitabs / 8;
        let bitrel = bitabs % 8;

        let namejs = match namejs {
            "final" => "r#final",
            _ => namejs,
        };
        let fnname = namejs.to_case(Case::Snake);

        writeln!(&mut serimpl, "        #[allow(clippy::identity_op)]")?;
        writeln!(
            &mut serimpl,
            "        if (self.0[{}] >> {}) & 0x1 == 1 {{",
            byte, bitrel
        )?;
        writeln!(
            &mut serimpl,
            "            v.push(\"{}\");",
            escape_str(&namejs)
        )?;
        writeln!(&mut serimpl, "        }}")?;

        writeln!(&mut modcode, "        #[allow(clippy::identity_op)]")?;
        writeln!(
            &mut modcode,
            "    pub fn {}(&mut self, v: bool) -> &mut Self {{",
            fnname
        )?;
        writeln!(
            &mut modcode,
            "        if v {{ self.0[{}] |= 1 << {}; }}",
            byte, bitrel
        )?;
        writeln!(
            &mut modcode,
            "        else {{ self.0[{}] &= !(1 << {}); }}",
            byte, bitrel
        )?;
        writeln!(&mut modcode, "        self")?;
        writeln!(&mut modcode, "    }}")?;
    }
    writeln!(
        &mut serimpl,
        "        serializer.serialize_str(&v.join(\"+\"))"
    )?;
    impl_serialize_end(&mut serimpl)?;
    enum_impl_end(&mut modcode)?;
    modcode.write_all(&serimpl)?;

    let mut serimpl: Vec<u8> = Vec::new();
    impl_serialize_start(&mut serimpl, &subtypename, false)?;
    writeln!(&mut serimpl, "        match self {{")?;

    writeln!(&mut modcode, "#[derive(Clone, Debug)]")?;
    writeln!(&mut modcode, "pub enum {} {{", subtypename)?;
    writeln!(&mut modcode, "    Flags({}),", subtypeflagsname)?;
    let nextras = gen_enum(&mut modcode, &mut serimpl, &attr["extras"])?;
    writeln!(&mut modcode, "}}")?;

    writeln!(
        &mut serimpl,
        "            Self::Flags(v) => v.serialize(serializer),"
    )?;
    writeln!(&mut serimpl, "        }}")?;
    impl_serialize_end(&mut serimpl)?;
    modcode.write_all(&serimpl)?;

    writeln!(&mut modcode, "impl Default for {} {{", subtypename)?;
    writeln!(&mut modcode, "    fn default() -> Self {{")?;
    writeln!(
        &mut modcode,
        "        Self::Flags({}::default())",
        subtypeflagsname
    )?;
    writeln!(&mut modcode, "    }}")?;
    writeln!(&mut modcode, "}}")?;

    enum_impl_start(&mut modcode, &subtypename, false)?;
    writeln!(
        &mut modcode,
        "    pub fn flags(&mut self) -> &mut {} {{",
        subtypeflagsname
    )?;
    writeln!(
        &mut modcode,
        "        *self = Self::Flags({}::default());",
        subtypeflagsname
    )?;
    writeln!(&mut modcode, "        match self {{")?;
    writeln!(&mut modcode, "            Self::Flags(v) => v,")?;
    if nextras > 0 {
        writeln!(&mut modcode, "            _ => unreachable!(),")?;
    }
    writeln!(&mut modcode, "        }}")?;
    writeln!(&mut modcode, "    }}")?;

    writeln!(
        &mut modcode,
        "    pub fn set(&mut self, v: {}) {{",
        subtypename
    )?;
    writeln!(&mut modcode, "        *self = v;")?;
    writeln!(&mut modcode, "    }}")?;
    enum_impl_end(&mut modcode)?;

    Ok(subtypename)
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
    let is_layout = modname.is_none() && structname == "Layout";

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

        let (use_isempty, typestr) = if let Some(valtype) = attr["valType"].as_str() {
            let (subtypename, use_isempty) = match valtype {
                "enumerated" => (
                    Some(format!(
                        "{}{}",
                        namespace,
                        handle_enumerated(&attrname, attr, &mut modcode)?
                    )),
                    false,
                ),
                "flaglist" => (
                    Some(format!(
                        "{}{}",
                        namespace,
                        handle_flaglist(&attrname, attr, &mut modcode)?
                    )),
                    true,
                ),
                "data_array" => (get_dataarray_type(&attrname), false),
                _ => (None, false),
            };

            let rusttype = RustType::new(valtype, subtypename);
            let (type_lifetimes, type_generics) =
                make_lt_and_g(&attrname, rusttype.num_lifetimes(), rusttype.num_generics());

            if !type_lifetimes.is_empty() && lifetimes.is_empty() {
                lifetimes.push("'a".to_string());
            }
            generics.extend_from_slice(&type_generics);

            let typestr = rusttype.to_str(Some(&type_lifetimes), Some(&type_generics));
            (use_isempty, typestr)
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

            if !type_lifetimes.is_empty() && lifetimes.is_empty() {
                lifetimes.push("'a".to_string());
            }
            generics.extend_from_slice(&type_generics);

            let typestr = format!("{}{}<{}>", namespace, substructname, params_joined);
            (true, typestr)
        } else {
            panic!(
                "unsupported member: {}/{} = {:?}",
                structname, attrname_js, attr
            );
        };

        if let Some(description) = attr["description"].as_str() {
            writeln!(&mut code, "    /// {}", description)?;
            writeln!(&mut code, "    ///")?;
        }
        if attr.has_key("dflt") {
            writeln!(&mut code, "    /// default: `{}`", attr["dflt"])?;
        }

        if use_isempty {
            let needs_array = is_layout
                && match attrname.as_ref() {
                    "xaxis" | "yaxis" | "coloraxis" => true,
                    _ => false,
                };

            if needs_array {
                writeln!(&mut fields, "    #[serde(flatten)]")?;
                writeln!(
                    &mut fields,
                    "    pub {}: std::collections::HashMap<String, {}>,",
                    attrname, typestr
                )?;

                writeln!(
                    &mut code,
                    "    pub fn {}(&mut self, id: usize) -> &mut {} {{",
                    attrname, typestr
                )?;
                writeln!(
                    &mut code,
                    "        let field = if id == 0 {{ \"{}\".to_string() }}",
                    attrname_js
                )?;
                writeln!(
                    &mut code,
                    "                    else {{ format!(\"{}{{}}\", id + 1) }};",
                    attrname_js
                )?;
                writeln!(
                    &mut code,
                    "        if self.{}.get(&field).is_none() {{",
                    attrname
                )?;
                writeln!(
                    &mut code,
                    "            self.{}.insert(field.clone(), {}::default());",
                    attrname,
                    typestr.replace("<'a>", "")
                )?;
                writeln!(&mut code, "        }}")?;
                writeln!(
                    &mut code,
                    "        self.{}.get_mut(&field).unwrap()",
                    attrname
                )?;
                writeln!(&mut code, "    }}")?;
            } else {
                writeln!(&mut fields, "    #[serde(rename = \"{}\")]", attrname_js)?;
                writeln!(
                    &mut fields,
                    "    #[serde(skip_serializing_if = \"crate::IsEmpty::is_empty\")]"
                )?;
                writeln!(
                    &mut fields,
                    "    pub {}: crate::IsEmpty<{}>,",
                    attrname, typestr
                )?;

                writeln!(
                    &mut code,
                    "    pub fn {}(&mut self) -> &mut {} {{",
                    attrname, typestr
                )?;
                writeln!(&mut code, "        self.{}.is_empty = false;", attrname)?;
                writeln!(&mut code, "        &mut self.{}.data", attrname)?;
                writeln!(&mut code, "    }}")?;
            }
        } else if typestr == "{STR}" {
            writeln!(&mut fields, "    #[serde(rename = \"{}\")]", attrname_js)?;
            writeln!(
                &mut fields,
                "    #[serde(skip_serializing_if = \"Option::is_none\")]"
            )?;
            writeln!(
                &mut fields,
                "    pub {}: Option<std::borrow::Cow<'a, str>>,",
                attrname
            )?;

            writeln!(
                &mut code,
                "    pub fn {}<T: Into<std::borrow::Cow<'a, str>>>(&mut self, {}: T) -> &mut Self {{",
                attrname, attrname
            )?;
            writeln!(
                &mut code,
                "        self.{} = Some({}.into());",
                attrname, attrname
            )?;
            writeln!(&mut code, "        self")?;
            writeln!(&mut code, "    }}")?;
        } else {
            writeln!(&mut fields, "    #[serde(rename = \"{}\")]", attrname_js)?;
            writeln!(
                &mut fields,
                "    #[serde(skip_serializing_if = \"Option::is_none\")]"
            )?;
            writeln!(&mut fields, "    pub {}: Option<{}>,", attrname, typestr)?;

            writeln!(
                &mut code,
                "    pub fn {}(&mut self, {}: {}) -> &mut Self {{",
                attrname, attrname, typestr
            )?;
            writeln!(&mut code, "        self.{} = Some({});", attrname, attrname)?;
            writeln!(&mut code, "        self")?;
            writeln!(&mut code, "    }}")?;
        }
    }

    let params_joined = [&lifetimes[..], &generics[..]].concat().join(", ");

    if modname.is_none() {
        writeln!(f, "#[allow(unused_imports)]")?;
        writeln!(f, "use serde::Serialize;")?;
    }
    writeln!(f)?;
    if let Some(description) = description {
        writeln!(f, "/// {}", description)?;
    }
    writeln!(f, "#[derive(Default, Clone, Debug, Serialize)]")?;
    writeln!(f, "pub struct {}<{}> {{", structname, params_joined)?;
    f.write_all(&fields)?;
    writeln!(f, "}}")?;
    writeln!(f)?;

    writeln!(f, "#[allow(clippy::wrong_self_convention)]")?;
    writeln!(f, "#[allow(clippy::ptr_arg)]")?;
    writeln!(
        f,
        "impl<{}> {}<{}> {{",
        params_joined, structname, params_joined
    )?;
    f.write_all(&code)?;
    writeln!(f, "}}")?;

    if !modcode.is_empty() {
        if let Some(modname) = &modname {
            writeln!(f, "pub mod {} {{", modname)?;
            writeln!(f, "#[allow(unused_imports)]")?;
            writeln!(f, "use serde::Serialize;")?;
        }

        f.write_all(&modcode)?;

        if modname.is_some() {
            writeln!(f, "}}")?;
        }
    }

    Ok((lifetimes.len(), generics.len()))
}

fn main() -> Result<(), Error> {
    let pkg_version = env!("CARGO_PKG_VERSION");
    let plotly_version = pkg_version.splitn(2, '-').next().unwrap();
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

    writeln!(
        &mut f_mod,
        "/// https CDN URL to the plotly version that was used to build this crate"
    )?;
    writeln!(
        &mut f_mod,
        "pub static URL_CDN: &str = \"https://cdn.plot.ly/plotly-{}.min.js\";",
        plotly_version
    )?;

    println!("cargo:rerun-if-changed=build.rs");

    Ok(())
}
