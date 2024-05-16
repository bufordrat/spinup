let scrambled = "bqj_pekjatpajoekj]hepu<ci]eh*_ki"
let factor = 4
let adjust n c = Char.(chr (code c + n))
let email = String.map (adjust factor) scrambled
