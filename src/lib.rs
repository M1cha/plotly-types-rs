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

include!(concat!(env!("OUT_DIR"), "/mod.rs"));

#[cfg(test)]
mod test {
    use super::*;
    use traces::scatter::Scatter;

    #[test]
    fn blah() {
        let x = vec![0.0f64];

        let mut trace: Scatter<(), _, _> = Scatter::default();
        trace.x(&x).y(&x);
        println!("{}", serde_json::to_string(&trace).unwrap());

        /*
        let mut t = crate::traces::heatmap::Heatmap::default();
        let mut cs = vec![crate::ColorScaleElem::new(1.0, "green")];

        t.colorscale(crate::ColorScale::Array(&cs));
        println!("{}", serde_json::to_string(&t).unwrap());*/
    }
}
