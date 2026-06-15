pub static CONDITION_TABLE: ConditionTable = gen_condition_table();

type ConditionTable = [bool; 256];

const fn gen_condition_table() -> ConditionTable {
    let mut table = [false; 256];

    let mut i = 0;
    while i < 16 {
        let v = (i & 1) != 0;
        let c = (i & 2) != 0;
        let z = (i & 4) != 0;
        let n = (i & 8) != 0;

        table[i] = z;
        table[(1 << 4) | i] = !z;
        table[(2 << 4) | i] = c;
        table[(3 << 4) | i] = !c;
        table[(4 << 4) | i] = n;
        table[(5 << 4) | i] = !n;
        table[(6 << 4) | i] = v;
        table[(7 << 4) | i] = !v;
        table[(8 << 4) | i] = c && !z;
        table[(9 << 4) | i] = !c || z;
        table[(10 << 4) | i] = n == v;
        table[(11 << 4) | i] = n != v;
        table[(12 << 4) | i] = !z && (n == v);
        table[(13 << 4) | i] = z || (n != v);
        table[(14 << 4) | i] = true;
        table[(15 << 4) | i] = false;

        i += 1;
    }

    table
}
