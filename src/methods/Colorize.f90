


Function Colorize (This,Sub,ArgColor) Result(OutPut)
 	Implicit None 
  Class(Str)                           :: This 
  Character(len=*), Intent(In)         :: Sub
  Character(len=*), Intent(In)         :: ArgColor

  Type(Str)                            :: OutPut

  Type(Str)                            :: InputColor
  Type(Str), Dimension(:), Allocatable :: Color
  Character(len=:),Allocatable         :: Sub_
  Character(len=:),Allocatable         :: Test 
  Integer                              :: Args
  Integer                              :: i

  Type(Str)                            :: Default
  Type(Str)                            :: Bold
  Type(Str)                            :: Blink
  Type(Str)                            :: Green
  Type(Str)                            :: Red
  Type(Str)                            :: Yellow
  Type(Str)                            :: Blue
  Type(Str)                            :: Magenta
  Type(Str)                            :: Cyan

  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  ! Default Colors
  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  Default = achar(27) // "[0m"
  Bold    = achar(27) // "[1m"
  Red     = achar(27) // "[31m"
  Green   = achar(27) // "[32m"
  Yellow  = achar(27) // "[33m"
  Blue    = achar(27) // "[34m"
  Magenta = achar(27) // "[35m"
  Cyan    = achar(27) // "[36m"
  
  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  ! Test String
  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  !This is a Test in MD5 Hash
  Test    = 'ce114e4501d2f4e2dcea3e17b546f339'

  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  ! Split The ArgColor
  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  InputColor = Trim(Adjustl(ArgColor))
  InputColor = InputColor%LowerCase()

  Color      = InputColor%Split() 

  Args       = Size(Color)
  Sub_       = '' 
  Do i = 1, Args
         If(Color(i) == 'red'    ) Then ; Sub_ = Sub_ // Red    %Name 
    Else If(Color(i) == 'bold'   ) Then ; Sub_ = Sub_ // Bold   %Name 
    Else If(Color(i) == 'green'  ) Then ; Sub_ = Sub_ // Green  %Name 
    Else If(Color(i) == 'blink'  ) Then ; Sub_ = Sub_ // Blink  %Name 
    Else If(Color(i) == 'yellow' ) Then ; Sub_ = Sub_ // Yellow %Name 
    Else If(Color(i) == 'blue'   ) Then ; Sub_ = Sub_ // Blue   %Name 
    Else If(Color(i) == 'magenta') Then ; Sub_ = Sub_ // Magenta%Name 
    Else If(Color(i) == 'cyan'   ) Then ; Sub_ = Sub_ // Cyan   %Name 
    End  If 
  End Do 
  Sub_ = Sub_ // Test // Default%Name

  OutPut = This  %Replace(Sub  , Sub_ )
  OutPut = OutPut%Replace(Test , Sub  )
  


End Function Colorize