  
  
  
  Function       Replace (This,Old,New,Max)  Result(OutPut)
    Class(Str)                , Intent(In) :: This 
    Character(len=*)          , Intent(In) :: Old 
    Character(len=*)          , Intent(In) :: New
    Integer         , Optional, Intent(In) :: Max 

    Type(Str)                              :: OutPut

    Character(len=50000)                   :: Char_
    Integer                                :: LOld
    Integer                                :: LNew
    Integer                                :: i
    Integer                                :: Count
    Integer                                :: Max_


    Char_   = This%Name 
    LOld   = Len(Old) 
    LNew   = Len(New)

    OutPut = This

    Count  = 0
    Max_   = 0
    If(Present(Max)) Max_ = Max 



    Check: Do
      I = OutPut%FindOne(Old); IF (i == 0) Exit Check


      Char_ = Char_(1:i-1) // New // Char_(i+LOld:)

      OutPut%Name = Trim(Adjustl(Char_))

      Count = Count + 1 
      If (Present(Max) .and. Count == Max_) Exit Check      
    End Do Check

    OutPut%Name = Trim(Adjustl(OutPut%Name))
  End Function   Replace