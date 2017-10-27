
  Function       Multiplicity_Right_Int  (This,Int) Result(OutPut)
    Implicit None
    Class(Str) , Intent(In)       :: This 
    Integer    , Intent(In)       :: Int
    Type(Str)                     :: OutPut

    Character(len=:), Allocatable :: DummyChar
    Integer                       :: i


    DummyChar = ''

    Do i = 1 , Int
      DummyChar = DummyChar //This%Name
    End Do 

    OutPut%Name = DummyChar
  End Function   Multiplicity_Right_Int

  Function       Multiplicity_Left_Int   (Int,This) Result(OutPut)
    Implicit None
    Class(Str) , Intent(In)       :: This 
    Integer    , Intent(In)       :: Int
    Type(Str)                     :: OutPut

    Character(len=:), Allocatable :: DummyChar
    Integer                       :: i


    DummyChar = ''
    DummyChar = Trim(Adjustl(DummyChar))
    Do i = 1 , Int
      DummyChar = DummyChar //This%Name
    End Do 

    OutPut%Name = DummyChar
  End Function   Multiplicity_Left_Int



