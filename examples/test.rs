use types::typed;

struct Tuple<T>(pub String, T);

struct Named<T> {
    a: String,
    pub b: T,
}

struct NamedAndGen<T, U> {
    t: T,
    u: U,
}

typed! {
    Num<TY> {
        U8: u8,
        U16: u16,
        U32: u32,
        U64: u64,
        U128: u128,
    } for {
        Tuple<TY>,
        Named<TY>,
        NamedAndGen<TY, String>,
    }
}

fn main() {
    // Test that the generated enums compile
    let _base = Num::U8;
    let _tuple = TupleNum::U16(Tuple("test".to_string(), 42u16));
    let _named = NamedNum::U32(Named {
        a: "test".to_string(),
        b: 42u32,
    });
    println!("All enums generated successfully!");
}
