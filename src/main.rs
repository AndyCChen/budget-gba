fn main() {
    let op1: u8 = 0x01;
    let op2: u8 = 0x00;
    //let result = op1.wrapping_add((!op2).wrapping_add(1));

    //let overflow = ((result ^ op1) & (result ^ !op2) & 0x8) != 0;
    // println!("{result}, {overflow}");

    {
        let (result, carry) = op1.overflowing_sub(op2);
        println!("1. {result:08b} {}", !carry);
    }

    {
        let (op2, carry0) = (!op2).overflowing_add(u8::from(true));
        let (result, carry1) = op1.overflowing_add(op2);

        println!("2. {result:08b} {}", carry0 || carry1);
    }

    {
        let (result, carry) = op1.overflowing_add(op2);
        println!("3. {result:08b} {carry}");
    }
}
