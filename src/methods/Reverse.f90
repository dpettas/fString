  Function       Reverse(This) Result(OutPut)
    Implicit None
    Class(Str), Intent(In)        :: This
    Type (Str)                    :: OutPut

    Integer                       :: i 
    Character(len=:), Allocatable :: Char_
    Character(len=1)              :: Char1


    Char_ = ''

    Do i = len(This%Name), 1 ,-1
      Char1 =  This%Name(i:i)
      Char_ = Char_ // Char1 
    End Do 

    OutPut%Name = Char_
  End Function   Reverse
