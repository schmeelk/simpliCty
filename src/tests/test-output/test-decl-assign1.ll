; ModuleID = 'SimpliCty'

declare i32 @putchar(i32)

declare i32 @getchar(...)

define i32 @main() {
entry:
  %a = alloca i32
  store i32 65, i32* %a
  %b = alloca i32
  store i32 66, i32* %b
  %lv = load i32, i32* %a
  %putchar = call i32 @putchar(i32 %lv)
  %lv1 = load i32, i32* %b
  %putchar2 = call i32 @putchar(i32 %lv1)
  ret i32 0
}
