; ModuleID = 'SimpliCty'

declare i32 @putchar(i32)

declare i32 @getchar(...)

define i32 @main() {
entry:
  %x = alloca i32
  store i32 0, i32* %x
  %y = alloca i32
  store i32 0, i32* %y
  %z = alloca i32
  store i32 0, i32* %z
  store i32 0, i32* %x
  store i32 5, i32* %z
  %lv = load i32, i32* %x
  %binop = icmp eq i32 %lv, 0
  br i1 %binop, label %if.then, label %if.else6

if.else.merge:                                    ; preds = %if.else6, %if.else.merge3
  %lv7 = load i32, i32* %y
  %putchar8 = call i32 @putchar(i32 %lv7)
  ret i32 0

if.then:                                          ; preds = %entry
  %lv1 = load i32, i32* %z
  %binop2 = icmp eq i32 %lv1, 5
  br i1 %binop2, label %if.then4, label %if.else

if.else.merge3:                                   ; preds = %if.else, %if.then4
  br label %if.else.merge

if.then4:                                         ; preds = %if.then
  store i32 70, i32* %y
  %lv5 = load i32, i32* %y
  %putchar = call i32 @putchar(i32 %lv5)
  br label %if.else.merge3

if.else:                                          ; preds = %if.then
  store i32 75, i32* %y
  br label %if.else.merge3

if.else6:                                         ; preds = %entry
  store i32 70, i32* %y
  br label %if.else.merge
}
