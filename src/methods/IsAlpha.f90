
  Function       IsAlpha(This) Result(OutPut)
    Implicit None
    Class(Str), Intent(In) :: This
    Logical                :: OutPut




    OutPut = Verify(This%Name,'0123456789') == 0
    OutPut = .Not. OutPut
  End Function   IsAlpha