use serde::Serialize;

#[derive(Serialize)]
pub struct ColorScaleElem<'s>(f64, &'s str);

impl<'s> ColorScaleElem<'s> {
    pub fn new(level: f64, color: &'s str) -> Self {
        Self(level, color)
    }
}

#[derive(Serialize)]
pub enum ColorScaleName {
    Greys,
    YlGnBu,
    Greens,
    YlOrRd,
    Bluered,
    RdBu,
    Reds,
    Blues,
    Picnic,
    Rainbow,
    Portland,
    Jet,
    Hot,
    Blackbody,
    Earth,
    Electric,
    Viridis,
    Cividis,
}

#[derive(Serialize)]
#[serde(untagged)]
pub enum ColorScale<'a> {
    Name(ColorScaleName),
    Array(&'a [ColorScaleElem<'a>]),
}

pub type Any = serde_json::Value;
pub type InfoArray = Vec<serde_json::Value>;
pub type Angle = f64;

pub struct IsEmpty<T> {
    pub data: T,
    pub is_empty: bool,
}

impl<T: serde::ser::Serialize> serde::ser::Serialize for IsEmpty<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.data.serialize(serializer)
    }
}

impl<T: Default> Default for IsEmpty<T> {
    fn default() -> Self {
        Self {
            data: T::default(),
            is_empty: true,
        }
    }
}

impl<T> IsEmpty<T> {
    pub fn is_empty(&self) -> bool {
        self.is_empty
    }
}

include!(concat!(env!("OUT_DIR"), "/mod.rs"));
mod error;
pub use error::Error;

pub struct Plot<'a, W, D> {
    w: W,
    graph_div: D,
    layout: layout::Layout<'a>,
    config: config::Config<'a>,
    ntraces: usize,
}

impl<'a, W, D> Plot<'a, W, D>
where
    W: std::io::Write,
    D: AsRef<str>,
{
    pub fn new(mut w: W, graph_div: D) -> Result<Self, Error> {
        write!(w, "var data = [")?;
        Ok(Self {
            w,
            graph_div,
            layout: layout::Layout::default(),
            config: config::Config::default(),
            ntraces: 0,
        })
    }

    pub fn layout(&mut self) -> &mut layout::Layout<'a> {
        &mut self.layout
    }

    pub fn config(&mut self) -> &mut config::Config<'a> {
        &mut self.config
    }

    pub fn add_trace<T>(&mut self, trace: T) -> Result<(), Error>
    where
        T: serde::Serialize,
    {
        if self.ntraces > 0 {
            write!(self.w, ", ")?;
        }

        serde_json::to_writer(&mut self.w, &trace)?;
        self.ntraces += 1;

        Ok(())
    }

    pub fn finish(mut self) -> Result<W, Error> {
        writeln!(self.w, "];")?;

        write!(self.w, "var layout = ")?;
        serde_json::to_writer(&mut self.w, &self.layout)?;
        writeln!(self.w, ";")?;

        write!(self.w, "var config = ")?;
        serde_json::to_writer(&mut self.w, &self.config)?;
        writeln!(self.w, ";")?;

        writeln!(
            self.w,
            "function makeplot() {{ Plotly.newPlot(\"{}\", data, layout, config); }}",
            self.graph_div.as_ref()
        )?;

        Ok(self.w)
    }
}
