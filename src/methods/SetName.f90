  Subroutine     SetName(This,Name)
    ! Import Str 
    Character(len=*), Intent(In)          :: Name
    Class(Str)      , Intent(Out)         :: This

    This%Name  = Name

    End Subroutine SetName 
