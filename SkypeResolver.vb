Imports System.Net
Public Class Form1

    Dim wc As New WebClient

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
      WebRequest.Create("API Link" & TextBox1.Text)
      If TextBox1.Text = "" Then
      MsgBox("username pls")
      Else
      TextBox1.Text = wc.DownloadString("API Link" & TextBox1.Text & MsgBox("Here you go")
)
      End If
    End Sub

End Class
