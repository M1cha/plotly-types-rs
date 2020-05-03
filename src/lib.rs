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
pub enum ColorScale<'a, 's> {
    Name(ColorScaleName),
    Array(&'a [ColorScaleElem<'s>]),
}

#[derive(Serialize)]
pub struct Any {}

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