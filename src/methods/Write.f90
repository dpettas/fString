
  Subroutine     WriteStrUDIO (This, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)

    Class(Str)      , Intent(In)      :: This
    Integer         , Intent(In)      :: UNIT
    Character(*)    , Intent(In)      :: IOTYPE
    Integer         , Intent(In)      :: V_LIST (:)
    Integer         , Intent(Out)     :: IOSTAT
    Character(*)    , Intent(InOut)   :: IOMSG
    Character(len=:), Allocatable     :: Temp


    Allocate(Temp,Source=This%Name)
    
    Write (UNIT, FMT='(a)', IOSTAT=IOSTAT, IOMSG=IOMSG) Temp

    DeAllocate(Temp)
  End SUBROUTINE WriteStrUDIO
