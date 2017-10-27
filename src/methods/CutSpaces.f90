  Function     CutSpaces(This) Result(OutPut)
    Implicit None 
    Class(Str), Intent(In) :: This
    Type (Str)             :: OutPut


    OutPut%Name = Trim(Adjustl(This%Name))
  End Function CutSpaces

