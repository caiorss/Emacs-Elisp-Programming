
open System.Windows.Forms
open System.Drawing 

let f = new Form () 
f.Text <- "Hello world"
f.Name <- "Form1"
f.BackColor <- Color.Blue

let btn = new Button (Text= "Click Me", BackColor = Color.Green)
f.Controls.Add(btn)
btn.Click.Subscribe(fun _ -> printfn "Click me again")

Application.Run(f)
