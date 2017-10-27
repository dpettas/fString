

  Function       IsNumeric(This) Result(OutPut)
    Implicit None
    Class(Str), Intent(In) :: This
    Logical                :: OutPut

    If      (This%Count("D") > 1 .Or.This%Count("E") > 1 .Or. This%Count(".") > 1)Then 
    OutPut = .False.
    Return
    End If 

    OutPut = Verify(This%Name,'0123456789.-+EDde') == 0
  End Function   IsNumeric