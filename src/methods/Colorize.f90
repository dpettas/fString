


Function Colorize (This,Sub,Color) Result(OutPut)
 	Implicit None 
  Class(Str)                   :: This 
  Character(len=*), Intent(In) :: Sub
  Character(len=*), Intent(In) :: Color

  Type(Str)                    :: OutPut

  Type(Str)                    :: Color_
  Type(Str)                    :: InputColor
  Character(len=:),Allocatable :: Sub_
  Character(len=:),Allocatable :: Test 

  Type(Str)                    :: Blink
  Type(Str)                    :: Yellow
  Type(Str)                    :: Blue
  Type(Str)                    :: Magenta
  Type(Str)                    :: Cyan





  Bold    = achar(27) + "[1m"
  Red     = achar(27) + "[31m"
  Green   = achar(27) + "[32m"
  Yellow  = achar(27) + "[33m"
  Blue    = achar(27) + "[34m"
  Magenta = achar(27) + "[35m"
  Cyan    = achar(27) + "[36m"
  
  Default = achar(27) + "[0m"

  Test    = ' a!d@A#e$1%f^b&g*B(h)2ickCl3m '



  InputColor = Trim(Adjustl(Color))
  InputColor = InputColor%LowerCase()

  
       If(InputColor%Name == 'red'    ) Then ; Sub_ = Red  %Name + Test + Default%Name
  Else If(InputColor%Name == 'bold'   ) Then ; Sub_ = Bold %Name + Test + Default%Name 
  Else If(InputColor%Name == 'green'  ) Then ; Sub_ = Green%Name + Test + Default%Name 
  Else If(InputColor%Name == 'blink'  ) Then ; Sub_ = Blink%Name + Test + Default%Name 
  Else If(InputColor%Name == 'yellow' ) Then ; Sub_ = Blink%Name + Test + Default%Name 
  Else If(InputColor%Name == 'blue'   ) Then ; Sub_ = Blink%Name + Test + Default%Name 
  Else If(InputColor%Name == 'magenta') Then ; Sub_ = Blink%Name + Test + Default%Name 
  Else If(InputColor%Name == 'cyan'   ) Then ; Sub_ = Blink%Name + Test + Default%Name 
  End  If 

  OutPut = This  %Replace(Sub  , Sub_ )
  OutPut = OutPut%Replace(Test , Sub  )


End Function Colorize