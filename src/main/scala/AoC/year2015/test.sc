val input = "113322113".toList
val char: Char = input.head
val idx = input.indexWhere(_ != char)
val nextInput: List[Char] = input.drop(idx)