Imports System, System.IO, System.Collections.Generic
Imports System.Drawing, System.Drawing.Drawing2D
Imports System.ComponentModel, System.Windows.Forms
Imports System.Runtime.InteropServices
Imports System.Drawing.Imaging
Imports System.Reflection


#Region "Themebase"
'------------------
'Creator: aeonhack
'Site: elitevs.net
'Created: 08/02/2011
'Changed: 12/06/2011
'Version: 1.5.4
'------------------

MustInherit Class ThemeContainer154
    Inherits ContainerControl

#Region " Initialization "

    Protected G As Graphics, B As Bitmap

    Sub New()
        SetStyle(DirectCast(139270, ControlStyles), True)

        _ImageSize = Size.Empty
        Font = New Font("Verdana", 8S)

        MeasureBitmap = New Bitmap(1, 1)
        MeasureGraphics = Graphics.FromImage(MeasureBitmap)

        DrawRadialPath = New GraphicsPath

        InvalidateCustimization()
    End Sub

    Protected NotOverridable Overrides Sub OnHandleCreated(ByVal e As EventArgs)
        If DoneCreation Then InitializeMessages()

        InvalidateCustimization()
        ColorHook()

        If Not _LockWidth = 0 Then Width = _LockWidth
        If Not _LockHeight = 0 Then Height = _LockHeight
        If Not _ControlMode Then MyBase.Dock = DockStyle.Fill

        Transparent = _Transparent
        If _Transparent AndAlso _BackColor Then BackColor = Color.Transparent

        MyBase.OnHandleCreated(e)
    End Sub

    Private DoneCreation As Boolean
    Protected NotOverridable Overrides Sub OnParentChanged(ByVal e As EventArgs)
        MyBase.OnParentChanged(e)

        If Parent Is Nothing Then Return
        _IsParentForm = TypeOf Parent Is Form

        If Not _ControlMode Then
            InitializeMessages()

            If _IsParentForm Then
                ParentForm.FormBorderStyle = _BorderStyle
                ParentForm.TransparencyKey = _TransparencyKey

                If Not DesignMode Then
                    AddHandler ParentForm.Shown, AddressOf FormShown
                End If
            End If

            Parent.BackColor = BackColor
        End If

        OnCreation()
        DoneCreation = True
        InvalidateTimer()
    End Sub

#End Region

    Private Sub DoAnimation(ByVal i As Boolean)
        OnAnimation()
        If i Then Invalidate()
    End Sub

    Protected NotOverridable Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        If Width = 0 OrElse Height = 0 Then Return

        If _Transparent AndAlso _ControlMode Then
            PaintHook()
            e.Graphics.DrawImage(B, 0, 0)
        Else
            G = e.Graphics
            PaintHook()
        End If
    End Sub

    Protected Overrides Sub OnHandleDestroyed(ByVal e As EventArgs)
        RemoveAnimationCallback(AddressOf DoAnimation)
        MyBase.OnHandleDestroyed(e)
    End Sub

    Private HasShown As Boolean
    Private Sub FormShown(ByVal sender As Object, ByVal e As EventArgs)
        If _ControlMode OrElse HasShown Then Return

        If _StartPosition = FormStartPosition.CenterParent OrElse _StartPosition = FormStartPosition.CenterScreen Then
            Dim SB As Rectangle = Screen.PrimaryScreen.Bounds
            Dim CB As Rectangle = ParentForm.Bounds
            ParentForm.Location = New Point(SB.Width \ 2 - CB.Width \ 2, SB.Height \ 2 - CB.Width \ 2)
        End If

        HasShown = True
    End Sub


#Region " Size Handling "

    Private Frame As Rectangle
    Protected NotOverridable Overrides Sub OnSizeChanged(ByVal e As EventArgs)
        If _Movable AndAlso Not _ControlMode Then
            Frame = New Rectangle(7, 7, Width - 14, _Header - 7)
        End If

        InvalidateBitmap()
        Invalidate()

        MyBase.OnSizeChanged(e)
    End Sub

    Protected Overrides Sub SetBoundsCore(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal specified As BoundsSpecified)
        If Not _LockWidth = 0 Then width = _LockWidth
        If Not _LockHeight = 0 Then height = _LockHeight
        MyBase.SetBoundsCore(x, y, width, height, specified)
    End Sub

#End Region

#Region " State Handling "

    Protected State As MouseState
    Private Sub SetState(ByVal current As MouseState)
        State = current
        Invalidate()
    End Sub

    Protected Overrides Sub OnMouseMove(ByVal e As MouseEventArgs)
        If Not (_IsParentForm AndAlso ParentForm.WindowState = FormWindowState.Maximized) Then
            If _Sizable AndAlso Not _ControlMode Then InvalidateMouse()
        End If

        MyBase.OnMouseMove(e)
    End Sub

    Protected Overrides Sub OnEnabledChanged(ByVal e As EventArgs)
        If Enabled Then SetState(MouseState.None) Else SetState(MouseState.Block)
        MyBase.OnEnabledChanged(e)
    End Sub

    Protected Overrides Sub OnMouseEnter(ByVal e As EventArgs)
        SetState(MouseState.Over)
        MyBase.OnMouseEnter(e)
    End Sub

    Protected Overrides Sub OnMouseUp(ByVal e As MouseEventArgs)
        SetState(MouseState.Over)
        MyBase.OnMouseUp(e)
    End Sub

    Protected Overrides Sub OnMouseLeave(ByVal e As EventArgs)
        SetState(MouseState.None)

        If GetChildAtPoint(PointToClient(MousePosition)) IsNot Nothing Then
            If _Sizable AndAlso Not _ControlMode Then
                Cursor = Cursors.Default
                Previous = 0
            End If
        End If

        MyBase.OnMouseLeave(e)
    End Sub

    Protected Overrides Sub OnMouseDown(ByVal e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left Then SetState(MouseState.Down)

        If Not (_IsParentForm AndAlso ParentForm.WindowState = FormWindowState.Maximized OrElse _ControlMode) Then
            If _Movable AndAlso Frame.Contains(e.Location) Then
                Capture = False
                WM_LMBUTTONDOWN = True
                DefWndProc(Messages(0))
            ElseIf _Sizable AndAlso Not Previous = 0 Then
                Capture = False
                WM_LMBUTTONDOWN = True
                DefWndProc(Messages(Previous))
            End If
        End If

        MyBase.OnMouseDown(e)
    End Sub

    Private WM_LMBUTTONDOWN As Boolean
    Protected Overrides Sub WndProc(ByRef m As Message)
        MyBase.WndProc(m)

        If WM_LMBUTTONDOWN AndAlso m.Msg = 513 Then
            WM_LMBUTTONDOWN = False

            SetState(MouseState.Over)
            If Not _SmartBounds Then Return

            If IsParentMdi Then
                CorrectBounds(New Rectangle(Point.Empty, Parent.Parent.Size))
            Else
                CorrectBounds(Screen.FromControl(Parent).WorkingArea)
            End If
        End If
    End Sub

    Private GetIndexPoint As Point
    Private B1, B2, B3, B4 As Boolean
    Private Function GetIndex() As Integer
        GetIndexPoint = PointToClient(MousePosition)
        B1 = GetIndexPoint.X < 7
        B2 = GetIndexPoint.X > Width - 7
        B3 = GetIndexPoint.Y < 7
        B4 = GetIndexPoint.Y > Height - 7

        If B1 AndAlso B3 Then Return 4
        If B1 AndAlso B4 Then Return 7
        If B2 AndAlso B3 Then Return 5
        If B2 AndAlso B4 Then Return 8
        If B1 Then Return 1
        If B2 Then Return 2
        If B3 Then Return 3
        If B4 Then Return 6
        Return 0
    End Function

    Private Current, Previous As Integer
    Private Sub InvalidateMouse()
        Current = GetIndex()
        If Current = Previous Then Return

        Previous = Current
        Select Case Previous
            Case 0
                Cursor = Cursors.Default
            Case 1, 2
                Cursor = Cursors.SizeWE
            Case 3, 6
                Cursor = Cursors.SizeNS
            Case 4, 8
                Cursor = Cursors.SizeNWSE
            Case 5, 7
                Cursor = Cursors.SizeNESW
        End Select
    End Sub

    Private Messages(8) As Message
    Private Sub InitializeMessages()
        Messages(0) = Message.Create(Parent.Handle, 161, New IntPtr(2), IntPtr.Zero)
        For I As Integer = 1 To 8
            Messages(I) = Message.Create(Parent.Handle, 161, New IntPtr(I + 9), IntPtr.Zero)
        Next
    End Sub

    Private Sub CorrectBounds(ByVal bounds As Rectangle)
        If Parent.Width > bounds.Width Then Parent.Width = bounds.Width
        If Parent.Height > bounds.Height Then Parent.Height = bounds.Height

        Dim X As Integer = Parent.Location.X
        Dim Y As Integer = Parent.Location.Y

        If X < bounds.X Then X = bounds.X
        If Y < bounds.Y Then Y = bounds.Y

        Dim Width As Integer = bounds.X + bounds.Width
        Dim Height As Integer = bounds.Y + bounds.Height

        If X + Parent.Width > Width Then X = Width - Parent.Width
        If Y + Parent.Height > Height Then Y = Height - Parent.Height

        Parent.Location = New Point(X, Y)
    End Sub

#End Region


#Region " Base Properties "

    Overrides Property Dock As DockStyle
        Get
            Return MyBase.Dock
        End Get
        Set(ByVal value As DockStyle)
            If Not _ControlMode Then Return
            MyBase.Dock = value
        End Set
    End Property

    Private _BackColor As Boolean
    <Category("Misc")> _
    Overrides Property BackColor() As Color
        Get
            Return MyBase.BackColor
        End Get
        Set(ByVal value As Color)
            If value = MyBase.BackColor Then Return

            If Not IsHandleCreated AndAlso _ControlMode AndAlso value = Color.Transparent Then
                _BackColor = True
                Return
            End If

            MyBase.BackColor = value
            If Parent IsNot Nothing Then
                If Not _ControlMode Then Parent.BackColor = value
                ColorHook()
            End If
        End Set
    End Property

    Overrides Property MinimumSize As Size
        Get
            Return MyBase.MinimumSize
        End Get
        Set(ByVal value As Size)
            MyBase.MinimumSize = value
            If Parent IsNot Nothing Then Parent.MinimumSize = value
        End Set
    End Property

    Overrides Property MaximumSize As Size
        Get
            Return MyBase.MaximumSize
        End Get
        Set(ByVal value As Size)
            MyBase.MaximumSize = value
            If Parent IsNot Nothing Then Parent.MaximumSize = value
        End Set
    End Property

    Overrides Property Text() As String
        Get
            Return MyBase.Text
        End Get
        Set(ByVal value As String)
            MyBase.Text = value
            Invalidate()
        End Set
    End Property

    Overrides Property Font() As Font
        Get
            Return MyBase.Font
        End Get
        Set(ByVal value As Font)
            MyBase.Font = value
            Invalidate()
        End Set
    End Property

    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)> _
    Overrides Property ForeColor() As Color
        Get
            Return Color.Empty
        End Get
        Set(ByVal value As Color)
        End Set
    End Property
    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)> _
    Overrides Property BackgroundImage() As Image
        Get
            Return Nothing
        End Get
        Set(ByVal value As Image)
        End Set
    End Property
    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)> _
    Overrides Property BackgroundImageLayout() As ImageLayout
        Get
            Return ImageLayout.None
        End Get
        Set(ByVal value As ImageLayout)
        End Set
    End Property

#End Region

#Region " Public Properties "

    Private _SmartBounds As Boolean = True
    Property SmartBounds() As Boolean
        Get
            Return _SmartBounds
        End Get
        Set(ByVal value As Boolean)
            _SmartBounds = value
        End Set
    End Property

    Private _Movable As Boolean = True
    Property Movable() As Boolean
        Get
            Return _Movable
        End Get
        Set(ByVal value As Boolean)
            _Movable = value
        End Set
    End Property

    Private _Sizable As Boolean = True
    Property Sizable() As Boolean
        Get
            Return _Sizable
        End Get
        Set(ByVal value As Boolean)
            _Sizable = value
        End Set
    End Property

    Private _TransparencyKey As Color
    Property TransparencyKey() As Color
        Get
            If _IsParentForm AndAlso Not _ControlMode Then Return ParentForm.TransparencyKey Else Return _TransparencyKey
        End Get
        Set(ByVal value As Color)
            If value = _TransparencyKey Then Return
            _TransparencyKey = value

            If _IsParentForm AndAlso Not _ControlMode Then
                ParentForm.TransparencyKey = value
                ColorHook()
            End If
        End Set
    End Property

    Private _BorderStyle As FormBorderStyle
    Property BorderStyle() As FormBorderStyle
        Get
            If _IsParentForm AndAlso Not _ControlMode Then Return ParentForm.FormBorderStyle Else Return _BorderStyle
        End Get
        Set(ByVal value As FormBorderStyle)
            _BorderStyle = value

            If _IsParentForm AndAlso Not _ControlMode Then
                ParentForm.FormBorderStyle = value

                If Not value = FormBorderStyle.None Then
                    Movable = False
                    Sizable = False
                End If
            End If
        End Set
    End Property

    Private _StartPosition As FormStartPosition
    Property StartPosition As FormStartPosition
        Get
            If _IsParentForm AndAlso Not _ControlMode Then Return ParentForm.StartPosition Else Return _StartPosition
        End Get
        Set(ByVal value As FormStartPosition)
            _StartPosition = value

            If _IsParentForm AndAlso Not _ControlMode Then
                ParentForm.StartPosition = value
            End If
        End Set
    End Property

    Private _NoRounding As Boolean
    Property NoRounding() As Boolean
        Get
            Return _NoRounding
        End Get
        Set(ByVal v As Boolean)
            _NoRounding = v
            Invalidate()
        End Set
    End Property

    Private _Image As Image
    Property Image() As Image
        Get
            Return _Image
        End Get
        Set(ByVal value As Image)
            If value Is Nothing Then _ImageSize = Size.Empty Else _ImageSize = value.Size

            _Image = value
            Invalidate()
        End Set
    End Property

    Private Items As New Dictionary(Of String, Color)
    Property Colors() As Bloom()
        Get
            Dim T As New List(Of Bloom)
            Dim E As Dictionary(Of String, Color).Enumerator = Items.GetEnumerator

            While E.MoveNext
                T.Add(New Bloom(E.Current.Key, E.Current.Value))
            End While

            Return T.ToArray
        End Get
        Set(ByVal value As Bloom())
            For Each B As Bloom In value
                If Items.ContainsKey(B.Name) Then Items(B.Name) = B.Value
            Next

            InvalidateCustimization()
            ColorHook()
            Invalidate()
        End Set
    End Property

    Private _Customization As String
    Property Customization() As String
        Get
            Return _Customization
        End Get
        Set(ByVal value As String)
            If value = _Customization Then Return

            Dim Data As Byte()
            Dim Items As Bloom() = Colors

            Try
                Data = Convert.FromBase64String(value)
                For I As Integer = 0 To Items.Length - 1
                    Items(I).Value = Color.FromArgb(BitConverter.ToInt32(Data, I * 4))
                Next
            Catch
                Return
            End Try

            _Customization = value

            Colors = Items
            ColorHook()
            Invalidate()
        End Set
    End Property

    Private _Transparent As Boolean
    Property Transparent() As Boolean
        Get
            Return _Transparent
        End Get
        Set(ByVal value As Boolean)
            _Transparent = value
            If Not (IsHandleCreated OrElse _ControlMode) Then Return

            If Not value AndAlso Not BackColor.A = 255 Then
                Throw New Exception("Unable to change value to false while a transparent BackColor is in use.")
            End If

            SetStyle(ControlStyles.Opaque, Not value)
            SetStyle(ControlStyles.SupportsTransparentBackColor, value)

            InvalidateBitmap()
            Invalidate()
        End Set
    End Property

#End Region

#Region " Private Properties "

    Private _ImageSize As Size
    Protected ReadOnly Property ImageSize() As Size
        Get
            Return _ImageSize
        End Get
    End Property

    Private _IsParentForm As Boolean
    Protected ReadOnly Property IsParentForm As Boolean
        Get
            Return _IsParentForm
        End Get
    End Property

    Protected ReadOnly Property IsParentMdi As Boolean
        Get
            If Parent Is Nothing Then Return False
            Return Parent.Parent IsNot Nothing
        End Get
    End Property

    Private _LockWidth As Integer
    Protected Property LockWidth() As Integer
        Get
            Return _LockWidth
        End Get
        Set(ByVal value As Integer)
            _LockWidth = value
            If Not LockWidth = 0 AndAlso IsHandleCreated Then Width = LockWidth
        End Set
    End Property

    Private _LockHeight As Integer
    Protected Property LockHeight() As Integer
        Get
            Return _LockHeight
        End Get
        Set(ByVal value As Integer)
            _LockHeight = value
            If Not LockHeight = 0 AndAlso IsHandleCreated Then Height = LockHeight
        End Set
    End Property

    Private _Header As Integer = 24
    Protected Property Header() As Integer
        Get
            Return _Header
        End Get
        Set(ByVal v As Integer)
            _Header = v

            If Not _ControlMode Then
                Frame = New Rectangle(7, 7, Width - 14, v - 7)
                Invalidate()
            End If
        End Set
    End Property

    Private _ControlMode As Boolean
    Protected Property ControlMode() As Boolean
        Get
            Return _ControlMode
        End Get
        Set(ByVal v As Boolean)
            _ControlMode = v

            Transparent = _Transparent
            If _Transparent AndAlso _BackColor Then BackColor = Color.Transparent

            InvalidateBitmap()
            Invalidate()
        End Set
    End Property

    Private _IsAnimated As Boolean
    Protected Property IsAnimated() As Boolean
        Get
            Return _IsAnimated
        End Get
        Set(ByVal value As Boolean)
            _IsAnimated = value
            InvalidateTimer()
        End Set
    End Property

#End Region


#Region " Property Helpers "

    Protected Function GetPen(ByVal name As String) As Pen
        Return New Pen(Items(name))
    End Function
    Protected Function GetPen(ByVal name As String, ByVal width As Single) As Pen
        Return New Pen(Items(name), width)
    End Function

    Protected Function GetBrush(ByVal name As String) As SolidBrush
        Return New SolidBrush(Items(name))
    End Function

    Protected Function GetColor(ByVal name As String) As Color
        Return Items(name)
    End Function

    Protected Sub SetColor(ByVal name As String, ByVal value As Color)
        If Items.ContainsKey(name) Then Items(name) = value Else Items.Add(name, value)
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        SetColor(name, Color.FromArgb(r, g, b))
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal a As Byte, ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        SetColor(name, Color.FromArgb(a, r, g, b))
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal a As Byte, ByVal value As Color)
        SetColor(name, Color.FromArgb(a, value))
    End Sub

    Private Sub InvalidateBitmap()
        If _Transparent AndAlso _ControlMode Then
            If Width = 0 OrElse Height = 0 Then Return
            B = New Bitmap(Width, Height, PixelFormat.Format32bppPArgb)
            G = Graphics.FromImage(B)
        Else
            G = Nothing
            B = Nothing
        End If
    End Sub

    Private Sub InvalidateCustimization()
        Dim M As New MemoryStream(Items.Count * 4)

        For Each B As Bloom In Colors
            M.Write(BitConverter.GetBytes(B.Value.ToArgb), 0, 4)
        Next

        M.Close()
        _Customization = Convert.ToBase64String(M.ToArray)
    End Sub

    Private Sub InvalidateTimer()
        If DesignMode OrElse Not DoneCreation Then Return

        If _IsAnimated Then
            AddAnimationCallback(AddressOf DoAnimation)
        Else
            RemoveAnimationCallback(AddressOf DoAnimation)
        End If
    End Sub

#End Region


#Region " User Hooks "

    Protected MustOverride Sub ColorHook()
    Protected MustOverride Sub PaintHook()

    Protected Overridable Sub OnCreation()
    End Sub

    Protected Overridable Sub OnAnimation()
    End Sub

#End Region


#Region " Offset "

    Private OffsetReturnRectangle As Rectangle
    Protected Function Offset(ByVal r As Rectangle, ByVal amount As Integer) As Rectangle
        OffsetReturnRectangle = New Rectangle(r.X + amount, r.Y + amount, r.Width - (amount * 2), r.Height - (amount * 2))
        Return OffsetReturnRectangle
    End Function

    Private OffsetReturnSize As Size
    Protected Function Offset(ByVal s As Size, ByVal amount As Integer) As Size
        OffsetReturnSize = New Size(s.Width + amount, s.Height + amount)
        Return OffsetReturnSize
    End Function

    Private OffsetReturnPoint As Point
    Protected Function Offset(ByVal p As Point, ByVal amount As Integer) As Point
        OffsetReturnPoint = New Point(p.X + amount, p.Y + amount)
        Return OffsetReturnPoint
    End Function

#End Region

#Region " Center "

    Private CenterReturn As Point

    Protected Function Center(ByVal p As Rectangle, ByVal c As Rectangle) As Point
        CenterReturn = New Point((p.Width \ 2 - c.Width \ 2) + p.X + c.X, (p.Height \ 2 - c.Height \ 2) + p.Y + c.Y)
        Return CenterReturn
    End Function
    Protected Function Center(ByVal p As Rectangle, ByVal c As Size) As Point
        CenterReturn = New Point((p.Width \ 2 - c.Width \ 2) + p.X, (p.Height \ 2 - c.Height \ 2) + p.Y)
        Return CenterReturn
    End Function

    Protected Function Center(ByVal child As Rectangle) As Point
        Return Center(Width, Height, child.Width, child.Height)
    End Function
    Protected Function Center(ByVal child As Size) As Point
        Return Center(Width, Height, child.Width, child.Height)
    End Function
    Protected Function Center(ByVal childWidth As Integer, ByVal childHeight As Integer) As Point
        Return Center(Width, Height, childWidth, childHeight)
    End Function

    Protected Function Center(ByVal p As Size, ByVal c As Size) As Point
        Return Center(p.Width, p.Height, c.Width, c.Height)
    End Function

    Protected Function Center(ByVal pWidth As Integer, ByVal pHeight As Integer, ByVal cWidth As Integer, ByVal cHeight As Integer) As Point
        CenterReturn = New Point(pWidth \ 2 - cWidth \ 2, pHeight \ 2 - cHeight \ 2)
        Return CenterReturn
    End Function

#End Region

#Region " Measure "

    Private MeasureBitmap As Bitmap
    Private MeasureGraphics As Graphics

    Protected Function Measure() As Size
        SyncLock MeasureGraphics
            Return MeasureGraphics.MeasureString(Text, Font, Width).ToSize
        End SyncLock
    End Function
    Protected Function Measure(ByVal text As String) As Size
        SyncLock MeasureGraphics
            Return MeasureGraphics.MeasureString(text, Font, Width).ToSize
        End SyncLock
    End Function

#End Region


#Region " DrawPixel "

    Private DrawPixelBrush As SolidBrush

    Protected Sub DrawPixel(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer)
        If _Transparent Then
            B.SetPixel(x, y, c1)
        Else
            DrawPixelBrush = New SolidBrush(c1)
            G.FillRectangle(DrawPixelBrush, x, y, 1, 1)
        End If
    End Sub

#End Region

#Region " DrawCorners "

    Private DrawCornersBrush As SolidBrush

    Protected Sub DrawCorners(ByVal c1 As Color, ByVal offset As Integer)
        DrawCorners(c1, 0, 0, Width, Height, offset)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal r1 As Rectangle, ByVal offset As Integer)
        DrawCorners(c1, r1.X, r1.Y, r1.Width, r1.Height, offset)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal offset As Integer)
        DrawCorners(c1, x + offset, y + offset, width - (offset * 2), height - (offset * 2))
    End Sub

    Protected Sub DrawCorners(ByVal c1 As Color)
        DrawCorners(c1, 0, 0, Width, Height)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal r1 As Rectangle)
        DrawCorners(c1, r1.X, r1.Y, r1.Width, r1.Height)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        If _NoRounding Then Return

        If _Transparent Then
            B.SetPixel(x, y, c1)
            B.SetPixel(x + (width - 1), y, c1)
            B.SetPixel(x, y + (height - 1), c1)
            B.SetPixel(x + (width - 1), y + (height - 1), c1)
        Else
            DrawCornersBrush = New SolidBrush(c1)
            G.FillRectangle(DrawCornersBrush, x, y, 1, 1)
            G.FillRectangle(DrawCornersBrush, x + (width - 1), y, 1, 1)
            G.FillRectangle(DrawCornersBrush, x, y + (height - 1), 1, 1)
            G.FillRectangle(DrawCornersBrush, x + (width - 1), y + (height - 1), 1, 1)
        End If
    End Sub

#End Region

#Region " DrawBorders "

    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal offset As Integer)
        DrawBorders(p1, 0, 0, Width, Height, offset)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal r As Rectangle, ByVal offset As Integer)
        DrawBorders(p1, r.X, r.Y, r.Width, r.Height, offset)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal offset As Integer)
        DrawBorders(p1, x + offset, y + offset, width - (offset * 2), height - (offset * 2))
    End Sub

    Protected Sub DrawBorders(ByVal p1 As Pen)
        DrawBorders(p1, 0, 0, Width, Height)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal r As Rectangle)
        DrawBorders(p1, r.X, r.Y, r.Width, r.Height)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        G.DrawRectangle(p1, x, y, width - 1, height - 1)
    End Sub

#End Region

#Region " DrawText "

    Private DrawTextPoint As Point
    Private DrawTextSize As Size

    Protected Sub DrawText(ByVal b1 As Brush, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        DrawText(b1, Text, a, x, y)
    End Sub
    Protected Sub DrawText(ByVal b1 As Brush, ByVal text As String, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        If text.Length = 0 Then Return

        DrawTextSize = Measure(text)
        DrawTextPoint = New Point(Width \ 2 - DrawTextSize.Width \ 2, Header \ 2 - DrawTextSize.Height \ 2)

        Select Case a
            Case HorizontalAlignment.Left
                G.DrawString(text, Font, b1, x, DrawTextPoint.Y + y)
            Case HorizontalAlignment.Center
                G.DrawString(text, Font, b1, DrawTextPoint.X + x, DrawTextPoint.Y + y)
            Case HorizontalAlignment.Right
                G.DrawString(text, Font, b1, Width - DrawTextSize.Width - x, DrawTextPoint.Y + y)
        End Select
    End Sub

    Protected Sub DrawText(ByVal b1 As Brush, ByVal p1 As Point)
        If Text.Length = 0 Then Return
        G.DrawString(Text, Font, b1, p1)
    End Sub
    Protected Sub DrawText(ByVal b1 As Brush, ByVal x As Integer, ByVal y As Integer)
        If Text.Length = 0 Then Return
        G.DrawString(Text, Font, b1, x, y)
    End Sub

#End Region

#Region " DrawImage "

    Private DrawImagePoint As Point

    Protected Sub DrawImage(ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        DrawImage(_Image, a, x, y)
    End Sub
    Protected Sub DrawImage(ByVal image As Image, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        If image Is Nothing Then Return
        DrawImagePoint = New Point(Width \ 2 - image.Width \ 2, Header \ 2 - image.Height \ 2)

        Select Case a
            Case HorizontalAlignment.Left
                G.DrawImage(image, x, DrawImagePoint.Y + y, image.Width, image.Height)
            Case HorizontalAlignment.Center
                G.DrawImage(image, DrawImagePoint.X + x, DrawImagePoint.Y + y, image.Width, image.Height)
            Case HorizontalAlignment.Right
                G.DrawImage(image, Width - image.Width - x, DrawImagePoint.Y + y, image.Width, image.Height)
        End Select
    End Sub

    Protected Sub DrawImage(ByVal p1 As Point)
        DrawImage(_Image, p1.X, p1.Y)
    End Sub
    Protected Sub DrawImage(ByVal x As Integer, ByVal y As Integer)
        DrawImage(_Image, x, y)
    End Sub

    Protected Sub DrawImage(ByVal image As Image, ByVal p1 As Point)
        DrawImage(image, p1.X, p1.Y)
    End Sub
    Protected Sub DrawImage(ByVal image As Image, ByVal x As Integer, ByVal y As Integer)
        If image Is Nothing Then Return
        G.DrawImage(image, x, y, image.Width, image.Height)
    End Sub

#End Region

#Region " DrawGradient "

    Private DrawGradientBrush As LinearGradientBrush
    Private DrawGradientRectangle As Rectangle

    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(blend, DrawGradientRectangle)
    End Sub
    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(blend, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal r As Rectangle)
        DrawGradientBrush = New LinearGradientBrush(r, Color.Empty, Color.Empty, 90.0F)
        DrawGradientBrush.InterpolationColors = blend
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal angle As Single)
        DrawGradientBrush = New LinearGradientBrush(r, Color.Empty, Color.Empty, angle)
        DrawGradientBrush.InterpolationColors = blend
        G.FillRectangle(DrawGradientBrush, r)
    End Sub


    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(c1, c2, DrawGradientRectangle)
    End Sub
    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(c1, c2, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle)
        DrawGradientBrush = New LinearGradientBrush(r, c1, c2, 90.0F)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle, ByVal angle As Single)
        DrawGradientBrush = New LinearGradientBrush(r, c1, c2, angle)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub

#End Region

#Region " DrawRadial "

    Private DrawRadialPath As GraphicsPath
    Private DrawRadialBrush1 As PathGradientBrush
    Private DrawRadialBrush2 As LinearGradientBrush
    Private DrawRadialRectangle As Rectangle

    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, width \ 2, height \ 2)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal center As Point)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, center.X, center.Y)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal cx As Integer, ByVal cy As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, cx, cy)
    End Sub

    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle)
        DrawRadial(blend, r, r.Width \ 2, r.Height \ 2)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal center As Point)
        DrawRadial(blend, r, center.X, center.Y)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal cx As Integer, ByVal cy As Integer)
        DrawRadialPath.Reset()
        DrawRadialPath.AddEllipse(r.X, r.Y, r.Width - 1, r.Height - 1)

        DrawRadialBrush1 = New PathGradientBrush(DrawRadialPath)
        DrawRadialBrush1.CenterPoint = New Point(r.X + cx, r.Y + cy)
        DrawRadialBrush1.InterpolationColors = blend

        If G.SmoothingMode = SmoothingMode.AntiAlias Then
            G.FillEllipse(DrawRadialBrush1, r.X + 1, r.Y + 1, r.Width - 3, r.Height - 3)
        Else
            G.FillEllipse(DrawRadialBrush1, r)
        End If
    End Sub


    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(c1, c2, DrawGradientRectangle)
    End Sub
    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(c1, c2, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle)
        DrawRadialBrush2 = New LinearGradientBrush(r, c1, c2, 90.0F)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle, ByVal angle As Single)
        DrawRadialBrush2 = New LinearGradientBrush(r, c1, c2, angle)
        G.FillEllipse(DrawGradientBrush, r)
    End Sub

#End Region

#Region " CreateRound "

    Private CreateRoundPath As GraphicsPath
    Private CreateRoundRectangle As Rectangle

    Function CreateRound(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal slope As Integer) As GraphicsPath
        CreateRoundRectangle = New Rectangle(x, y, width, height)
        Return CreateRound(CreateRoundRectangle, slope)
    End Function

    Function CreateRound(ByVal r As Rectangle, ByVal slope As Integer) As GraphicsPath
        CreateRoundPath = New GraphicsPath(FillMode.Winding)
        CreateRoundPath.AddArc(r.X, r.Y, slope, slope, 180.0F, 90.0F)
        CreateRoundPath.AddArc(r.Right - slope, r.Y, slope, slope, 270.0F, 90.0F)
        CreateRoundPath.AddArc(r.Right - slope, r.Bottom - slope, slope, slope, 0.0F, 90.0F)
        CreateRoundPath.AddArc(r.X, r.Bottom - slope, slope, slope, 90.0F, 90.0F)
        CreateRoundPath.CloseFigure()
        Return CreateRoundPath
    End Function

#End Region

End Class

MustInherit Class ThemeControl154
    Inherits Control


#Region " Initialization "

    Protected G As Graphics, B As Bitmap

    Sub New()
        SetStyle(DirectCast(139270, ControlStyles), True)
        _ImageSize = Size.Empty
        Font = New Font("Verdana", 8S)

        MeasureBitmap = New Bitmap(1, 1)
        MeasureGraphics = Graphics.FromImage(MeasureBitmap)

        DrawRadialPath = New GraphicsPath

        InvalidateCustimization() 'Remove?
    End Sub

    Protected NotOverridable Overrides Sub OnHandleCreated(ByVal e As EventArgs)
        InvalidateCustimization()
        ColorHook()

        If Not _LockWidth = 0 Then Width = _LockWidth
        If Not _LockHeight = 0 Then Height = _LockHeight

        Transparent = _Transparent
        If _Transparent AndAlso _BackColor Then BackColor = Color.Transparent

        MyBase.OnHandleCreated(e)
    End Sub

    Private DoneCreation As Boolean
    Protected NotOverridable Overrides Sub OnParentChanged(ByVal e As EventArgs)
        If Parent IsNot Nothing Then
            OnCreation()
            DoneCreation = True
            InvalidateTimer()
        End If

        MyBase.OnParentChanged(e)
    End Sub

#End Region

    Private Sub DoAnimation(ByVal i As Boolean)
        OnAnimation()
        If i Then Invalidate()
    End Sub

    Protected NotOverridable Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        If Width = 0 OrElse Height = 0 Then Return

        If _Transparent Then
            PaintHook()
            e.Graphics.DrawImage(B, 0, 0)
        Else
            G = e.Graphics
            PaintHook()
        End If
    End Sub

    Protected Overrides Sub OnHandleDestroyed(ByVal e As EventArgs)
        RemoveAnimationCallback(AddressOf DoAnimation)
        MyBase.OnHandleDestroyed(e)
    End Sub

#Region " Size Handling "

    Protected NotOverridable Overrides Sub OnSizeChanged(ByVal e As EventArgs)
        If _Transparent Then
            InvalidateBitmap()
        End If

        Invalidate()
        MyBase.OnSizeChanged(e)
    End Sub

    Protected Overrides Sub SetBoundsCore(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal specified As BoundsSpecified)
        If Not _LockWidth = 0 Then width = _LockWidth
        If Not _LockHeight = 0 Then height = _LockHeight
        MyBase.SetBoundsCore(x, y, width, height, specified)
    End Sub

#End Region

#Region " State Handling "

    Private InPosition As Boolean
    Protected Overrides Sub OnMouseEnter(ByVal e As EventArgs)
        InPosition = True
        SetState(MouseState.Over)
        MyBase.OnMouseEnter(e)
    End Sub

    Protected Overrides Sub OnMouseUp(ByVal e As MouseEventArgs)
        If InPosition Then SetState(MouseState.Over)
        MyBase.OnMouseUp(e)
    End Sub

    Protected Overrides Sub OnMouseDown(ByVal e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left Then SetState(MouseState.Down)
        MyBase.OnMouseDown(e)
    End Sub

    Protected Overrides Sub OnMouseLeave(ByVal e As EventArgs)
        InPosition = False
        SetState(MouseState.None)
        MyBase.OnMouseLeave(e)
    End Sub

    Protected Overrides Sub OnEnabledChanged(ByVal e As EventArgs)
        If Enabled Then SetState(MouseState.None) Else SetState(MouseState.Block)
        MyBase.OnEnabledChanged(e)
    End Sub

    Protected State As MouseState
    Private Sub SetState(ByVal current As MouseState)
        State = current
        Invalidate()
    End Sub

#End Region


#Region " Base Properties "

    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)> _
    Overrides Property ForeColor() As Color
        Get
            Return Color.Empty
        End Get
        Set(ByVal value As Color)
        End Set
    End Property
    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)> _
    Overrides Property BackgroundImage() As Image
        Get
            Return Nothing
        End Get
        Set(ByVal value As Image)
        End Set
    End Property
    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)> _
    Overrides Property BackgroundImageLayout() As ImageLayout
        Get
            Return ImageLayout.None
        End Get
        Set(ByVal value As ImageLayout)
        End Set
    End Property

    Overrides Property Text() As String
        Get
            Return MyBase.Text
        End Get
        Set(ByVal value As String)
            MyBase.Text = value
            Invalidate()
        End Set
    End Property
    Overrides Property Font() As Font
        Get
            Return MyBase.Font
        End Get
        Set(ByVal value As Font)
            MyBase.Font = value
            Invalidate()
        End Set
    End Property

    Private _BackColor As Boolean
    <Category("Misc")> _
    Overrides Property BackColor() As Color
        Get
            Return MyBase.BackColor
        End Get
        Set(ByVal value As Color)
            If Not IsHandleCreated AndAlso value = Color.Transparent Then
                _BackColor = True
                Return
            End If

            MyBase.BackColor = value
            If Parent IsNot Nothing Then ColorHook()
        End Set
    End Property

#End Region

#Region " Public Properties "

    Private _NoRounding As Boolean
    Property NoRounding() As Boolean
        Get
            Return _NoRounding
        End Get
        Set(ByVal v As Boolean)
            _NoRounding = v
            Invalidate()
        End Set
    End Property

    Private _Image As Image
    Property Image() As Image
        Get
            Return _Image
        End Get
        Set(ByVal value As Image)
            If value Is Nothing Then
                _ImageSize = Size.Empty
            Else
                _ImageSize = value.Size
            End If

            _Image = value
            Invalidate()
        End Set
    End Property

    Private _Transparent As Boolean
    Property Transparent() As Boolean
        Get
            Return _Transparent
        End Get
        Set(ByVal value As Boolean)
            _Transparent = value
            If Not IsHandleCreated Then Return

            If Not value AndAlso Not BackColor.A = 255 Then
                Throw New Exception("Unable to change value to false while a transparent BackColor is in use.")
            End If

            SetStyle(ControlStyles.Opaque, Not value)
            SetStyle(ControlStyles.SupportsTransparentBackColor, value)

            If value Then InvalidateBitmap() Else B = Nothing
            Invalidate()
        End Set
    End Property

    Private Items As New Dictionary(Of String, Color)
    Property Colors() As Bloom()
        Get
            Dim T As New List(Of Bloom)
            Dim E As Dictionary(Of String, Color).Enumerator = Items.GetEnumerator

            While E.MoveNext
                T.Add(New Bloom(E.Current.Key, E.Current.Value))
            End While

            Return T.ToArray
        End Get
        Set(ByVal value As Bloom())
            For Each B As Bloom In value
                If Items.ContainsKey(B.Name) Then Items(B.Name) = B.Value
            Next

            InvalidateCustimization()
            ColorHook()
            Invalidate()
        End Set
    End Property

    Private _Customization As String
    Property Customization() As String
        Get
            Return _Customization
        End Get
        Set(ByVal value As String)
            If value = _Customization Then Return

            Dim Data As Byte()
            Dim Items As Bloom() = Colors

            Try
                Data = Convert.FromBase64String(value)
                For I As Integer = 0 To Items.Length - 1
                    Items(I).Value = Color.FromArgb(BitConverter.ToInt32(Data, I * 4))
                Next
            Catch
                Return
            End Try

            _Customization = value

            Colors = Items
            ColorHook()
            Invalidate()
        End Set
    End Property

#End Region

#Region " Private Properties "

    Private _ImageSize As Size
    Protected ReadOnly Property ImageSize() As Size
        Get
            Return _ImageSize
        End Get
    End Property

    Private _LockWidth As Integer
    Protected Property LockWidth() As Integer
        Get
            Return _LockWidth
        End Get
        Set(ByVal value As Integer)
            _LockWidth = value
            If Not LockWidth = 0 AndAlso IsHandleCreated Then Width = LockWidth
        End Set
    End Property

    Private _LockHeight As Integer
    Protected Property LockHeight() As Integer
        Get
            Return _LockHeight
        End Get
        Set(ByVal value As Integer)
            _LockHeight = value
            If Not LockHeight = 0 AndAlso IsHandleCreated Then Height = LockHeight
        End Set
    End Property

    Private _IsAnimated As Boolean
    Protected Property IsAnimated() As Boolean
        Get
            Return _IsAnimated
        End Get
        Set(ByVal value As Boolean)
            _IsAnimated = value
            InvalidateTimer()
        End Set
    End Property

#End Region


#Region " Property Helpers "

    Protected Function GetPen(ByVal name As String) As Pen
        Return New Pen(Items(name))
    End Function
    Protected Function GetPen(ByVal name As String, ByVal width As Single) As Pen
        Return New Pen(Items(name), width)
    End Function

    Protected Function GetBrush(ByVal name As String) As SolidBrush
        Return New SolidBrush(Items(name))
    End Function

    Protected Function GetColor(ByVal name As String) As Color
        Return Items(name)
    End Function

    Protected Sub SetColor(ByVal name As String, ByVal value As Color)
        If Items.ContainsKey(name) Then Items(name) = value Else Items.Add(name, value)
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        SetColor(name, Color.FromArgb(r, g, b))
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal a As Byte, ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        SetColor(name, Color.FromArgb(a, r, g, b))
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal a As Byte, ByVal value As Color)
        SetColor(name, Color.FromArgb(a, value))
    End Sub

    Private Sub InvalidateBitmap()
        If Width = 0 OrElse Height = 0 Then Return
        B = New Bitmap(Width, Height, PixelFormat.Format32bppPArgb)
        G = Graphics.FromImage(B)
    End Sub

    Private Sub InvalidateCustimization()
        Dim M As New MemoryStream(Items.Count * 4)

        For Each B As Bloom In Colors
            M.Write(BitConverter.GetBytes(B.Value.ToArgb), 0, 4)
        Next

        M.Close()
        _Customization = Convert.ToBase64String(M.ToArray)
    End Sub

    Private Sub InvalidateTimer()
        If DesignMode OrElse Not DoneCreation Then Return

        If _IsAnimated Then
            AddAnimationCallback(AddressOf DoAnimation)
        Else
            RemoveAnimationCallback(AddressOf DoAnimation)
        End If
    End Sub
#End Region


#Region " User Hooks "

    Protected MustOverride Sub ColorHook()
    Protected MustOverride Sub PaintHook()

    Protected Overridable Sub OnCreation()
    End Sub

    Protected Overridable Sub OnAnimation()
    End Sub

#End Region


#Region " Offset "

    Private OffsetReturnRectangle As Rectangle
    Protected Function Offset(ByVal r As Rectangle, ByVal amount As Integer) As Rectangle
        OffsetReturnRectangle = New Rectangle(r.X + amount, r.Y + amount, r.Width - (amount * 2), r.Height - (amount * 2))
        Return OffsetReturnRectangle
    End Function

    Private OffsetReturnSize As Size
    Protected Function Offset(ByVal s As Size, ByVal amount As Integer) As Size
        OffsetReturnSize = New Size(s.Width + amount, s.Height + amount)
        Return OffsetReturnSize
    End Function

    Private OffsetReturnPoint As Point
    Protected Function Offset(ByVal p As Point, ByVal amount As Integer) As Point
        OffsetReturnPoint = New Point(p.X + amount, p.Y + amount)
        Return OffsetReturnPoint
    End Function

#End Region

#Region " Center "

    Private CenterReturn As Point

    Protected Function Center(ByVal p As Rectangle, ByVal c As Rectangle) As Point
        CenterReturn = New Point((p.Width \ 2 - c.Width \ 2) + p.X + c.X, (p.Height \ 2 - c.Height \ 2) + p.Y + c.Y)
        Return CenterReturn
    End Function
    Protected Function Center(ByVal p As Rectangle, ByVal c As Size) As Point
        CenterReturn = New Point((p.Width \ 2 - c.Width \ 2) + p.X, (p.Height \ 2 - c.Height \ 2) + p.Y)
        Return CenterReturn
    End Function

    Protected Function Center(ByVal child As Rectangle) As Point
        Return Center(Width, Height, child.Width, child.Height)
    End Function
    Protected Function Center(ByVal child As Size) As Point
        Return Center(Width, Height, child.Width, child.Height)
    End Function
    Protected Function Center(ByVal childWidth As Integer, ByVal childHeight As Integer) As Point
        Return Center(Width, Height, childWidth, childHeight)
    End Function

    Protected Function Center(ByVal p As Size, ByVal c As Size) As Point
        Return Center(p.Width, p.Height, c.Width, c.Height)
    End Function

    Protected Function Center(ByVal pWidth As Integer, ByVal pHeight As Integer, ByVal cWidth As Integer, ByVal cHeight As Integer) As Point
        CenterReturn = New Point(pWidth \ 2 - cWidth \ 2, pHeight \ 2 - cHeight \ 2)
        Return CenterReturn
    End Function

#End Region

#Region " Measure "

    Private MeasureBitmap As Bitmap
    Private MeasureGraphics As Graphics 'TODO: Potential issues during multi-threading.

    Protected Function Measure() As Size
        Return MeasureGraphics.MeasureString(Text, Font, Width).ToSize
    End Function
    Protected Function Measure(ByVal text As String) As Size
        Return MeasureGraphics.MeasureString(text, Font, Width).ToSize
    End Function

#End Region


#Region " DrawPixel "

    Private DrawPixelBrush As SolidBrush

    Protected Sub DrawPixel(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer)
        If _Transparent Then
            B.SetPixel(x, y, c1)
        Else
            DrawPixelBrush = New SolidBrush(c1)
            G.FillRectangle(DrawPixelBrush, x, y, 1, 1)
        End If
    End Sub

#End Region

#Region " DrawCorners "

    Private DrawCornersBrush As SolidBrush

    Protected Sub DrawCorners(ByVal c1 As Color, ByVal offset As Integer)
        DrawCorners(c1, 0, 0, Width, Height, offset)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal r1 As Rectangle, ByVal offset As Integer)
        DrawCorners(c1, r1.X, r1.Y, r1.Width, r1.Height, offset)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal offset As Integer)
        DrawCorners(c1, x + offset, y + offset, width - (offset * 2), height - (offset * 2))
    End Sub

    Protected Sub DrawCorners(ByVal c1 As Color)
        DrawCorners(c1, 0, 0, Width, Height)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal r1 As Rectangle)
        DrawCorners(c1, r1.X, r1.Y, r1.Width, r1.Height)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        If _NoRounding Then Return

        If _Transparent Then
            B.SetPixel(x, y, c1)
            B.SetPixel(x + (width - 1), y, c1)
            B.SetPixel(x, y + (height - 1), c1)
            B.SetPixel(x + (width - 1), y + (height - 1), c1)
        Else
            DrawCornersBrush = New SolidBrush(c1)
            G.FillRectangle(DrawCornersBrush, x, y, 1, 1)
            G.FillRectangle(DrawCornersBrush, x + (width - 1), y, 1, 1)
            G.FillRectangle(DrawCornersBrush, x, y + (height - 1), 1, 1)
            G.FillRectangle(DrawCornersBrush, x + (width - 1), y + (height - 1), 1, 1)
        End If
    End Sub

#End Region

#Region " DrawBorders "

    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal offset As Integer)
        DrawBorders(p1, 0, 0, Width, Height, offset)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal r As Rectangle, ByVal offset As Integer)
        DrawBorders(p1, r.X, r.Y, r.Width, r.Height, offset)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal offset As Integer)
        DrawBorders(p1, x + offset, y + offset, width - (offset * 2), height - (offset * 2))
    End Sub

    Protected Sub DrawBorders(ByVal p1 As Pen)
        DrawBorders(p1, 0, 0, Width, Height)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal r As Rectangle)
        DrawBorders(p1, r.X, r.Y, r.Width, r.Height)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        G.DrawRectangle(p1, x, y, width - 1, height - 1)
    End Sub

#End Region

#Region " DrawText "

    Private DrawTextPoint As Point
    Private DrawTextSize As Size

    Protected Sub DrawText(ByVal b1 As Brush, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        DrawText(b1, Text, a, x, y)
    End Sub
    Protected Sub DrawText(ByVal b1 As Brush, ByVal text As String, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        If text.Length = 0 Then Return

        DrawTextSize = Measure(text)
        DrawTextPoint = Center(DrawTextSize)

        Select Case a
            Case HorizontalAlignment.Left
                G.DrawString(text, Font, b1, x, DrawTextPoint.Y + y)
            Case HorizontalAlignment.Center
                G.DrawString(text, Font, b1, DrawTextPoint.X + x, DrawTextPoint.Y + y)
            Case HorizontalAlignment.Right
                G.DrawString(text, Font, b1, Width - DrawTextSize.Width - x, DrawTextPoint.Y + y)
        End Select
    End Sub

    Protected Sub DrawText(ByVal b1 As Brush, ByVal p1 As Point)
        If Text.Length = 0 Then Return
        G.DrawString(Text, Font, b1, p1)
    End Sub
    Protected Sub DrawText(ByVal b1 As Brush, ByVal x As Integer, ByVal y As Integer)
        If Text.Length = 0 Then Return
        G.DrawString(Text, Font, b1, x, y)
    End Sub

#End Region

#Region " DrawImage "

    Private DrawImagePoint As Point

    Protected Sub DrawImage(ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        DrawImage(_Image, a, x, y)
    End Sub
    Protected Sub DrawImage(ByVal image As Image, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        If image Is Nothing Then Return
        DrawImagePoint = Center(image.Size)

        Select Case a
            Case HorizontalAlignment.Left
                G.DrawImage(image, x, DrawImagePoint.Y + y, image.Width, image.Height)
            Case HorizontalAlignment.Center
                G.DrawImage(image, DrawImagePoint.X + x, DrawImagePoint.Y + y, image.Width, image.Height)
            Case HorizontalAlignment.Right
                G.DrawImage(image, Width - image.Width - x, DrawImagePoint.Y + y, image.Width, image.Height)
        End Select
    End Sub

    Protected Sub DrawImage(ByVal p1 As Point)
        DrawImage(_Image, p1.X, p1.Y)
    End Sub
    Protected Sub DrawImage(ByVal x As Integer, ByVal y As Integer)
        DrawImage(_Image, x, y)
    End Sub

    Protected Sub DrawImage(ByVal image As Image, ByVal p1 As Point)
        DrawImage(image, p1.X, p1.Y)
    End Sub
    Protected Sub DrawImage(ByVal image As Image, ByVal x As Integer, ByVal y As Integer)
        If image Is Nothing Then Return
        G.DrawImage(image, x, y, image.Width, image.Height)
    End Sub

#End Region

#Region " DrawGradient "

    Private DrawGradientBrush As LinearGradientBrush
    Private DrawGradientRectangle As Rectangle

    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(blend, DrawGradientRectangle)
    End Sub
    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(blend, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal r As Rectangle)
        DrawGradientBrush = New LinearGradientBrush(r, Color.Empty, Color.Empty, 90.0F)
        DrawGradientBrush.InterpolationColors = blend
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal angle As Single)
        DrawGradientBrush = New LinearGradientBrush(r, Color.Empty, Color.Empty, angle)
        DrawGradientBrush.InterpolationColors = blend
        G.FillRectangle(DrawGradientBrush, r)
    End Sub


    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(c1, c2, DrawGradientRectangle)
    End Sub
    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(c1, c2, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle)
        DrawGradientBrush = New LinearGradientBrush(r, c1, c2, 90.0F)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle, ByVal angle As Single)
        DrawGradientBrush = New LinearGradientBrush(r, c1, c2, angle)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub

#End Region

#Region " DrawRadial "

    Private DrawRadialPath As GraphicsPath
    Private DrawRadialBrush1 As PathGradientBrush
    Private DrawRadialBrush2 As LinearGradientBrush
    Private DrawRadialRectangle As Rectangle

    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, width \ 2, height \ 2)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal center As Point)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, center.X, center.Y)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal cx As Integer, ByVal cy As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, cx, cy)
    End Sub

    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle)
        DrawRadial(blend, r, r.Width \ 2, r.Height \ 2)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal center As Point)
        DrawRadial(blend, r, center.X, center.Y)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal cx As Integer, ByVal cy As Integer)
        DrawRadialPath.Reset()
        DrawRadialPath.AddEllipse(r.X, r.Y, r.Width - 1, r.Height - 1)

        DrawRadialBrush1 = New PathGradientBrush(DrawRadialPath)
        DrawRadialBrush1.CenterPoint = New Point(r.X + cx, r.Y + cy)
        DrawRadialBrush1.InterpolationColors = blend

        If G.SmoothingMode = SmoothingMode.AntiAlias Then
            G.FillEllipse(DrawRadialBrush1, r.X + 1, r.Y + 1, r.Width - 3, r.Height - 3)
        Else
            G.FillEllipse(DrawRadialBrush1, r)
        End If
    End Sub


    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(c1, c2, DrawRadialRectangle)
    End Sub
    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(c1, c2, DrawRadialRectangle, angle)
    End Sub

    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle)
        DrawRadialBrush2 = New LinearGradientBrush(r, c1, c2, 90.0F)
        G.FillEllipse(DrawRadialBrush2, r)
    End Sub
    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle, ByVal angle As Single)
        DrawRadialBrush2 = New LinearGradientBrush(r, c1, c2, angle)
        G.FillEllipse(DrawRadialBrush2, r)
    End Sub

#End Region

#Region " CreateRound "

    Private CreateRoundPath As GraphicsPath
    Private CreateRoundRectangle As Rectangle

    Function CreateRound(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal slope As Integer) As GraphicsPath
        CreateRoundRectangle = New Rectangle(x, y, width, height)
        Return CreateRound(CreateRoundRectangle, slope)
    End Function

    Function CreateRound(ByVal r As Rectangle, ByVal slope As Integer) As GraphicsPath
        CreateRoundPath = New GraphicsPath(FillMode.Winding)
        CreateRoundPath.AddArc(r.X, r.Y, slope, slope, 180.0F, 90.0F)
        CreateRoundPath.AddArc(r.Right - slope, r.Y, slope, slope, 270.0F, 90.0F)
        CreateRoundPath.AddArc(r.Right - slope, r.Bottom - slope, slope, slope, 0.0F, 90.0F)
        CreateRoundPath.AddArc(r.X, r.Bottom - slope, slope, slope, 90.0F, 90.0F)
        CreateRoundPath.CloseFigure()
        Return CreateRoundPath
    End Function

#End Region

End Class

Module ThemeShare

#Region " Animation "

    Private Frames As Integer
    Private Invalidate As Boolean
    Public ThemeTimer As New PrecisionTimer

    Private Const FPS As Integer = 50 '1000 / 50 = 20 FPS
    Private Const Rate As Integer = 10

    Public Delegate Sub AnimationDelegate(ByVal invalidate As Boolean)

    Private Callbacks As New List(Of AnimationDelegate)

    Private Sub HandleCallbacks(ByVal state As IntPtr, ByVal reserve As Boolean)
        Invalidate = (Frames >= FPS)
        If Invalidate Then Frames = 0

        SyncLock Callbacks
            For I As Integer = 0 To Callbacks.Count - 1
                Callbacks(I).Invoke(Invalidate)
            Next
        End SyncLock

        Frames += Rate
    End Sub

    Private Sub InvalidateThemeTimer()
        If Callbacks.Count = 0 Then
            ThemeTimer.Delete()
        Else
            ThemeTimer.Create(0, Rate, AddressOf HandleCallbacks)
        End If
    End Sub

    Sub AddAnimationCallback(ByVal callback As AnimationDelegate)
        SyncLock Callbacks
            If Callbacks.Contains(callback) Then Return

            Callbacks.Add(callback)
            InvalidateThemeTimer()
        End SyncLock
    End Sub

    Sub RemoveAnimationCallback(ByVal callback As AnimationDelegate)
        SyncLock Callbacks
            If Not Callbacks.Contains(callback) Then Return

            Callbacks.Remove(callback)
            InvalidateThemeTimer()
        End SyncLock
    End Sub

#End Region

End Module

Enum MouseState As Byte
    None = 0
    Over = 1
    Down = 2
    Block = 3
End Enum

Structure Bloom

    Public _Name As String
    ReadOnly Property Name() As String
        Get
            Return _Name
        End Get
    End Property

    Private _Value As Color
    Property Value() As Color
        Get
            Return _Value
        End Get
        Set(ByVal value As Color)
            _Value = value
        End Set
    End Property

    Property ValueHex() As String
        Get
            Return String.Concat("#", _
            _Value.R.ToString("X2", Nothing), _
            _Value.G.ToString("X2", Nothing), _
            _Value.B.ToString("X2", Nothing))
        End Get
        Set(ByVal value As String)
            Try
                _Value = ColorTranslator.FromHtml(value)
            Catch
                Return
            End Try
        End Set
    End Property


    Sub New(ByVal name As String, ByVal value As Color)
        _Name = name
        _Value = value
    End Sub
End Structure

'------------------
'Creator: aeonhack
'Site: elitevs.net
'Created: 11/30/2011
'Changed: 11/30/2011
'Version: 1.0.0
'------------------
Class PrecisionTimer
    Implements IDisposable

    Private _Enabled As Boolean
    ReadOnly Property Enabled() As Boolean
        Get
            Return _Enabled
        End Get
    End Property

    Private Handle As IntPtr
    Private TimerCallback As TimerDelegate

    <DllImport("kernel32.dll", EntryPoint:="CreateTimerQueueTimer")> _
    Private Shared Function CreateTimerQueueTimer( _
    ByRef handle As IntPtr, _
    ByVal queue As IntPtr, _
    ByVal callback As TimerDelegate, _
    ByVal state As IntPtr, _
    ByVal dueTime As UInteger, _
    ByVal period As UInteger, _
    ByVal flags As UInteger) As Boolean
    End Function

    <DllImport("kernel32.dll", EntryPoint:="DeleteTimerQueueTimer")> _
    Private Shared Function DeleteTimerQueueTimer( _
    ByVal queue As IntPtr, _
    ByVal handle As IntPtr, _
    ByVal callback As IntPtr) As Boolean
    End Function

    Delegate Sub TimerDelegate(ByVal r1 As IntPtr, ByVal r2 As Boolean)

    Sub Create(ByVal dueTime As UInteger, ByVal period As UInteger, ByVal callback As TimerDelegate)
        If _Enabled Then Return

        TimerCallback = callback
        Dim Success As Boolean = CreateTimerQueueTimer(Handle, IntPtr.Zero, TimerCallback, IntPtr.Zero, dueTime, period, 0)

        If Not Success Then ThrowNewException("CreateTimerQueueTimer")
        _Enabled = Success
    End Sub

    Sub Delete()
        If Not _Enabled Then Return
        Dim Success As Boolean = DeleteTimerQueueTimer(IntPtr.Zero, Handle, IntPtr.Zero)

        If Not Success AndAlso Not Marshal.GetLastWin32Error = 997 Then
            ThrowNewException("DeleteTimerQueueTimer")
        End If

        _Enabled = Not Success
    End Sub

    Private Sub ThrowNewException(ByVal name As String)
        Throw New Exception(String.Format("{0} failed. Win32Error: {1}", name, Marshal.GetLastWin32Error))
    End Sub

    Public Sub Dispose() Implements IDisposable.Dispose
        Delete()
    End Sub
End Class
#End Region

Class EvolveThemeControl
    Inherits ThemeContainer154
    Protected Overrides Sub ColorHook()
    End Sub

    Sub New()
        TransparencyKey = Color.Fuchsia
        MinimumSize = New Size(80, 55)
        Font = New Font("Segoe UI", 9)
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(Color.FromArgb(47, 47, 47))
        DrawBorders(New Pen(Color.FromArgb(104, 104, 104)), 1)
        Dim cblend As ColorBlend = New ColorBlend(2)
        cblend.Colors(0) = Color.FromArgb(66, 66, 66)
        cblend.Colors(1) = Color.FromArgb(50, 50, 50)
        cblend.Positions(0) = 0
        cblend.Positions(1) = 1
        DrawGradient(cblend, New Rectangle(New Point(2, 2), New Size(Me.Width - 4, 22)))
        G.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(2, 24), New Point(Me.Width - 3, 24))
        G.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(2, 25), New Point(Me.Width - 3, 25))
        DrawBorders(Pens.Black)
        DrawCorners(Color.Fuchsia)
        G.DrawIcon(Me.ParentForm.Icon, New Rectangle(New Point(8, 5), New Size(16, 16)))
        G.DrawString(Me.ParentForm.Text, Font, Brushes.White, New Point(28, 4))
    End Sub
End Class

Class EvolveButton
    Inherits ThemeControl154
    Protected Overrides Sub ColorHook()
    End Sub

    Public Sub DrawRoundedRectangle(ByVal g As Drawing.Graphics, ByVal r As Rectangle, ByVal d As Integer, ByVal p As Pen)
        Dim mode As Drawing2D.SmoothingMode = g.SmoothingMode
        g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias
        g.DrawArc(p, r.X, r.Y, d, d, 180, 90)
        g.DrawLine(p, CInt(r.X + d / 2), r.Y, CInt(r.X + r.Width - d / 2), r.Y)
        g.DrawArc(p, r.X + r.Width - d, r.Y, d, d, 270, 90)
        g.DrawLine(p, r.X, CInt(r.Y + d / 2), r.X, CInt(r.Y + r.Height - d / 2))
        g.DrawLine(p, CInt(r.X + r.Width), CInt(r.Y + d / 2), CInt(r.X + r.Width), CInt(r.Y + r.Height - d / 2))
        g.DrawLine(p, CInt(r.X + d / 2), CInt(r.Y + r.Height), CInt(r.X + r.Width - d / 2), CInt(r.Y + r.Height))
        g.DrawArc(p, r.X, r.Y + r.Height - d, d, d, 90, 90)
        g.DrawArc(p, r.X + r.Width - d, r.Y + r.Height - d, d, d, 0, 90)
        g.SmoothingMode = mode
    End Sub

    Sub New()
        Transparent = True
        Width = 100
        Height = 30
    End Sub

    Protected Overrides Sub PaintHook()
        If State = MouseState.None Then
            Dim cblend As New ColorBlend(2)
            cblend.Colors(0) = Color.FromArgb(88, 88, 88)
            cblend.Colors(1) = Color.FromArgb(20, 20, 20)
            cblend.Positions(0) = 0
            cblend.Positions(1) = 1
            G.FillRectangle(New SolidBrush(Color.FromArgb(34, 34, 34)), New Rectangle(New Point(4, 2), New Size(Width - 7, Me.Height - 4)))
            DrawGradient(cblend, New Rectangle(New Point(4, 2), New Size(Width - 7, Me.Height - 4)))
            cblend.Colors(0) = Color.FromArgb(55, 55, 55)
            cblend.Colors(1) = Color.FromArgb(21, 21, 21)
            DrawGradient(cblend, New Rectangle(New Point(4, (Height / 5) * 2), New Size(Width - 7, Height - 2 - ((Height / 5) * 2))), 90.0F)
            G.DrawLine(New Pen(Color.FromArgb(55, 55, 55)), New Point(4, (Height / 5) * 2), New Point(Width - 4, (Height / 5) * 2))
        ElseIf State = MouseState.Over Then
            Dim cblend As New ColorBlend(2)
            cblend.Colors(0) = Color.FromArgb(99, 99, 99)
            cblend.Colors(1) = Color.FromArgb(30, 30, 30)
            cblend.Positions(0) = 0
            cblend.Positions(1) = 1
            G.FillRectangle(New SolidBrush(Color.FromArgb(44, 44, 44)), New Rectangle(New Point(4, 2), New Size(Width - 7, Me.Height - 4)))
            DrawGradient(cblend, New Rectangle(New Point(4, 2), New Size(Width - 7, Me.Height - 4)))
            cblend.Colors(0) = Color.FromArgb(66, 66, 66)
            cblend.Colors(1) = Color.FromArgb(31, 31, 31)
            DrawGradient(cblend, New Rectangle(New Point(4, (Height / 5) * 2), New Size(Width - 7, Height - 2 - ((Height / 5) * 2))), 90.0F)
            G.DrawLine(New Pen(Color.FromArgb(66, 66, 66)), New Point(4, (Height / 5) * 2), New Point(Width - 4, (Height / 5) * 2))
        ElseIf State = MouseState.Down Then
            Dim cblend As New ColorBlend(2)
            cblend.Colors(0) = Color.FromArgb(77, 77, 77)
            cblend.Colors(1) = Color.FromArgb(10, 10, 10)
            cblend.Positions(0) = 0
            cblend.Positions(1) = 1
            G.FillRectangle(New SolidBrush(Color.FromArgb(24, 24, 24)), New Rectangle(New Point(4, 2), New Size(Width - 7, Me.Height - 4)))
            DrawGradient(cblend, New Rectangle(New Point(4, 2), New Size(Width - 7, Me.Height - 4)))
            cblend.Colors(0) = Color.FromArgb(44, 44, 44)
            cblend.Colors(1) = Color.FromArgb(11, 11, 11)
            DrawGradient(cblend, New Rectangle(New Point(4, (Height / 5) * 2), New Size(Width - 7, Height - 2 - ((Height / 5) * 2))), 90.0F)
            G.DrawLine(New Pen(Color.FromArgb(44, 44, 44)), New Point(4, (Height / 5) * 2), New Point(Width - 4, (Height / 5) * 2))
        End If

        Dim p As New Pen(Color.FromArgb(15, 15, 15))

        G.DrawLine(p, New Point(3, 3), New Point(3, Me.Height - 4))
        G.DrawLine(p, New Point(5, 1), New Point(Me.Width - 5, 1))
        G.DrawLine(p, New Point(Me.Width - 3, 3), New Point(Me.Width - 3, Me.Height - 4))
        G.DrawLine(p, New Point(5, Me.Height - 2), New Point(Me.Width - 5, Me.Height - 2))
        DrawPixel(Color.FromArgb(15, 15, 15), 4, 2)
        DrawPixel(Color.FromArgb(15, 15, 15), Me.Width - 4, 2)
        DrawPixel(Color.FromArgb(15, 15, 15), 4, Me.Height - 3)
        DrawPixel(Color.FromArgb(15, 15, 15), Me.Width - 4, Me.Height - 3)

        Dim textSize As SizeF = Me.CreateGraphics.MeasureString(Text, Font, Width - 4)
        Dim sf As New StringFormat
        sf.LineAlignment = StringAlignment.Center
        sf.Alignment = StringAlignment.Center
        G.DrawString(Text, Font, Brushes.Black, New RectangleF(1, 3, Me.Width - 5, Me.Height - 4), sf)
        G.DrawString(Text, Font, Brushes.Black, New RectangleF(3, 3, Me.Width - 5, Me.Height - 4), sf)
        G.DrawString(Text, Font, Brushes.White, New RectangleF(2, 2, Me.Width - 5, Me.Height - 4), sf)

    End Sub
End Class

Class EvolveProgressBar
    Inherits ThemeControl154

    Private _Value As Integer
    Public Property Value As Integer
        Get
            Return _Value
        End Get
        Set(V As Integer)
            If V >= Minimum And V <= _Max Then _Value = V
            Invalidate()
        End Set
    End Property

    Private _Max As Integer = 100
    Public Property Maximum As Integer
        Get
            Return _Max
        End Get
        Set(V As Integer)
            If V > _Min Then _Max = V
            Invalidate()
        End Set
    End Property

    Private _Min As Integer = 0
    Public Property Minimum As Integer
        Get
            Return _Min
        End Get
        Set(V As Integer)
            If V < _Max Then _Min = V
            Invalidate()
        End Set
    End Property

    Sub New()
        Transparent = True
    End Sub

    Protected Overrides Sub ColorHook()
    End Sub

    Protected Overrides Sub OnResize(e As System.EventArgs)
        MyBase.OnResize(e)
        Height = 11
    End Sub

    Protected Overrides Sub PaintHook()
        G.SmoothingMode = SmoothingMode.AntiAlias

        Dim Gbrush As New LinearGradientBrush(New Rectangle(New Point(6, 0), New Point(Width - 6, 10)), Color.FromArgb(10, 10, 10), Backcolor, 90.0F)
        G.FillRectangle(Gbrush, New Rectangle(New Point(6, 0), New Point(Width - 12, 10)))
        G.FillEllipse(Gbrush, New Rectangle(New Point(0, 0), New Size(10, 10)))
        G.FillEllipse(Gbrush, New Rectangle(New Point(Me.Width - 11, 0), New Size(10, 10)))
        If Value < 4 Then
            Gbrush = New LinearGradientBrush(New Rectangle(New Point(6, 0), New Point(Width - 6, 10)), Color.FromArgb(100, 100, 100), Color.FromArgb(80, 80, 80), 90.0F)
            G.FillEllipse(Gbrush, New Rectangle(New Point((Me.Width / 100) * _Value - 7, 0), New Size(5, 6)))
            Gbrush = New LinearGradientBrush(New Rectangle(New Point(6, 0), New Point(Width - 6, 10)), Color.FromArgb(70, 70, 70), Color.FromArgb(60, 60, 60), 90.0F)
            G.FillEllipse(Gbrush, New Rectangle(New Point((Me.Width / 100) * _Value - 7, 4), New Size(6, 6)))
            Dim Hatch As New HatchBrush(HatchStyle.WideUpwardDiagonal, Color.FromArgb(50, Color.Black), Color.Transparent)
            G.FillRectangle(Hatch, New Rectangle(New Point(2, 1), New Point((Me.Width / 100) * _Value - 2, 8)))
        Else
            Gbrush = New LinearGradientBrush(New Rectangle(New Point(6, 0), New Point(Width - 6, 10)), Color.FromArgb(100, 100, 100), Color.FromArgb(80, 80, 80), 90.0F)
            G.FillEllipse(Gbrush, New Rectangle(New Point((Me.Width / 100) * _Value - 7, 0), New Size(5, 6)))
            G.FillEllipse(Gbrush, New Rectangle(New Point(1, 1), New Size(9, 5)))
            G.FillRectangle(Gbrush, New Rectangle(New Point(7, 1), New Point((Me.Width / 100) * _Value - 11, 4)))
            Gbrush = New LinearGradientBrush(New Rectangle(New Point(6, 0), New Point(Width - 6, 10)), Color.FromArgb(70, 70, 70), Color.FromArgb(60, 60, 60), 90.0F)
            G.FillEllipse(Gbrush, New Rectangle(New Point((Me.Width / 100) * _Value - 7, 4), New Size(6, 6)))
            G.FillEllipse(Gbrush, New Rectangle(New Point(1, 5), New Size(9, 6)))
            G.FillRectangle(Gbrush, New Rectangle(New Point(7, 5), New Point((Me.Width / 100) * _Value - 11, 4)))
            Dim Hatch As New HatchBrush(HatchStyle.WideUpwardDiagonal, Color.FromArgb(50, Color.Black), Color.Transparent)
            G.FillRectangle(Hatch, New Rectangle(New Point(2, 1), New Point((Me.Width / 100) * _Value - 2, 8)))
        End If

        G.DrawArc(Pens.Black, New Rectangle(New Point(0, 0), New Size(10, 10)), -90, -180)
        G.DrawLine(Pens.Black, New Point(6, 0), New Point(Me.Width - 7, 0))
        G.DrawLine(Pens.Black, New Point(6, 10), New Point(Me.Width - 7, 10))
        G.DrawArc(Pens.Black, New Rectangle(New Point(Me.Width - 11, 0), New Size(10, 10)), 90, -180)
        G.DrawLine(New Pen(Color.FromArgb(72, 72, 72)), New Point(4, 11), New Point(Me.Width - 4, 11))
        G.DrawArc(Pens.Black, New Rectangle(New Point((Me.Width / 100) * _Value - 11, 0), New Size(10, 10)), 90, -180)

        DrawPixel(Backcolor, 0, 0)
        DrawPixel(Backcolor, 1, 0)
        DrawPixel(Backcolor, 2, 0)
        DrawPixel(Backcolor, 0, 1)
        DrawPixel(Backcolor, 1, 1)
        DrawPixel(Backcolor, 0, 2)
        DrawPixel(Backcolor, 1, 1)
        DrawPixel(Backcolor, 0, 3)
        DrawPixel(Backcolor, 0, 4)
        DrawPixel(Backcolor, 0, 9)
        DrawPixel(Backcolor, 0, 8)
        DrawPixel(Backcolor, 0, 10)
        DrawPixel(Backcolor, 1, 10)
        DrawPixel(Backcolor, 2, 10)
    End Sub
End Class

Class EvolveCheckBox
    Inherits ThemeControl154
    Private _Checked As Boolean
    Private X As Integer

    Protected Overrides Sub ColorHook()
    End Sub

    Public Property Checked As Boolean
        Get
            Return _Checked
        End Get
        Set(V As Boolean)
            _Checked = V
            Invalidate()
        End Set
    End Property

    Protected Overrides Sub OnMouseMove(e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(e)
        X = e.Location.X
        Invalidate()
    End Sub

    Protected Overrides Sub OnClick(e As System.EventArgs)
        MyBase.OnClick(e)
        If _Checked Then _Checked = False Else _Checked = True
    End Sub

    Sub New()
        Me.BackColor = Backcolor
    End Sub

    Protected Overrides Sub OnTextChanged(ByVal e As System.EventArgs)
        MyBase.OnTextChanged(e)
        Dim textSize As Integer
        textSize = Me.CreateGraphics.MeasureString(Text, Font).Width
        Me.Width = 20 + textSize
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(Backcolor)
        If Not Checked And State = MouseState.None Then
            Dim Gbrush As New LinearGradientBrush(New Rectangle(New Point(1, 1), New Point(14, 14)), Color.FromArgb(74, 74, 74), Color.FromArgb(22, 22, 22), 90.0F)
            G.FillRectangle(Gbrush, New Rectangle(New Point(1, 1), New Point(14, 14)))
            G.DrawLine(Pens.Black, 1, 0, 14, 0)
            G.DrawLine(Pens.Black, 0, 1, 0, 14)
            G.DrawLine(Pens.Black, 15, 1, 15, 14)
            G.DrawLine(Pens.Black, 1, 15, 14, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 15)
            DrawPixel(Color.FromArgb(85, 85, 85), 1, 1)
            DrawPixel(Color.FromArgb(85, 85, 85), 1, 2)
            DrawPixel(Color.FromArgb(85, 85, 85), 14, 1)
            DrawPixel(Color.FromArgb(85, 85, 85), 14, 2)
            DrawPixel(Color.FromArgb(16, 16, 16), 1, 14)
            DrawPixel(Color.FromArgb(16, 16, 16), 14, 14)
            G.DrawLine(New Pen(Color.FromArgb(110, 110, 110)), 2, 1, 13, 1)
        ElseIf Not Checked And State = MouseState.Over And X < 17 Then
            Dim Gbrush As New LinearGradientBrush(New Rectangle(New Point(1, 1), New Point(14, 14)), Color.FromArgb(80, 80, 80), Color.FromArgb(30, 30, 30), 90.0F)
            G.FillRectangle(Gbrush, New Rectangle(New Point(1, 1), New Point(14, 14)))
            G.DrawLine(Pens.Black, 1, 0, 14, 0)
            G.DrawLine(Pens.Black, 0, 1, 0, 14)
            G.DrawLine(Pens.Black, 15, 1, 15, 14)
            G.DrawLine(Pens.Black, 1, 15, 14, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 15)
            DrawPixel(Color.FromArgb(85, 85, 85), 1, 1)
            DrawPixel(Color.FromArgb(85, 85, 85), 1, 2)
            DrawPixel(Color.FromArgb(85, 85, 85), 14, 1)
            DrawPixel(Color.FromArgb(85, 85, 85), 14, 2)
            DrawPixel(Color.FromArgb(16, 16, 16), 1, 14)
            DrawPixel(Color.FromArgb(16, 16, 16), 14, 14)
            G.DrawLine(New Pen(Color.FromArgb(110, 110, 110)), 2, 1, 13, 1)
        ElseIf Checked And State = MouseState.None Then
            Dim Gbrush As New LinearGradientBrush(New Rectangle(New Point(1, 1), New Point(14, 14)), Color.FromArgb(120, 120, 120), Color.FromArgb(40, 40, 40), 90.0F)
            G.FillRectangle(Gbrush, New Rectangle(New Point(1, 1), New Point(14, 14)))
            G.DrawLine(Pens.Black, 1, 0, 14, 0)
            G.DrawLine(Pens.Black, 0, 1, 0, 14)
            G.DrawLine(Pens.Black, 15, 1, 15, 14)
            G.DrawLine(Pens.Black, 1, 15, 14, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 15)
            DrawPixel(Color.FromArgb(120, 120, 120), 1, 1)
            DrawPixel(Color.FromArgb(120, 120, 120), 1, 2)
            DrawPixel(Color.FromArgb(120, 120, 120), 14, 1)
            DrawPixel(Color.FromArgb(120, 120, 120), 14, 2)
            DrawPixel(Color.FromArgb(39, 39, 39), 1, 14)
            DrawPixel(Color.FromArgb(39, 39, 39), 14, 14)
            G.DrawLine(New Pen(Color.FromArgb(150, 150, 150)), 2, 1, 13, 1)
            G.DrawString("a", New Font("Webdings", 13), New SolidBrush(Color.FromArgb(12, 12, 12)), New Point(-3, -2))
            G.DrawString("a", New Font("Webdings", 13), Brushes.White, New Point(-3, -3))
        ElseIf Checked And State = MouseState.Over And X < 17 Then
            Dim Gbrush As New LinearGradientBrush(New Rectangle(New Point(1, 1), New Point(14, 14)), Color.FromArgb(135, 135, 135), Color.FromArgb(60, 60, 60), 90.0F)
            G.FillRectangle(Gbrush, New Rectangle(New Point(1, 1), New Point(14, 14)))
            G.DrawLine(Pens.Black, 1, 0, 14, 0)
            G.DrawLine(Pens.Black, 0, 1, 0, 14)
            G.DrawLine(Pens.Black, 15, 1, 15, 14)
            G.DrawLine(Pens.Black, 1, 15, 14, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 15)
            DrawPixel(Color.FromArgb(120, 120, 120), 1, 1)
            DrawPixel(Color.FromArgb(120, 120, 120), 1, 2)
            DrawPixel(Color.FromArgb(120, 120, 120), 14, 1)
            DrawPixel(Color.FromArgb(120, 120, 120), 14, 2)
            DrawPixel(Color.FromArgb(39, 39, 39), 1, 14)
            DrawPixel(Color.FromArgb(39, 39, 39), 14, 14)
            G.DrawLine(New Pen(Color.FromArgb(150, 150, 150)), 2, 1, 13, 1)
            G.DrawString("a", New Font("Webdings", 13), New SolidBrush(Color.FromArgb(12, 12, 12)), New Point(-3, -2))
            G.DrawString("a", New Font("Webdings", 13), Brushes.White, New Point(-3, -3))
        ElseIf Checked And State = MouseState.Over And X >= 17 Then
            Dim Gbrush As New LinearGradientBrush(New Rectangle(New Point(1, 1), New Point(14, 14)), Color.FromArgb(120, 120, 120), Color.FromArgb(40, 40, 40), 90.0F)
            G.FillRectangle(Gbrush, New Rectangle(New Point(1, 1), New Point(14, 14)))
            G.DrawLine(Pens.Black, 1, 0, 14, 0)
            G.DrawLine(Pens.Black, 0, 1, 0, 14)
            G.DrawLine(Pens.Black, 15, 1, 15, 14)
            G.DrawLine(Pens.Black, 1, 15, 14, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 15)
            DrawPixel(Color.FromArgb(120, 120, 120), 1, 1)
            DrawPixel(Color.FromArgb(120, 120, 120), 1, 2)
            DrawPixel(Color.FromArgb(120, 120, 120), 14, 1)
            DrawPixel(Color.FromArgb(120, 120, 120), 14, 2)
            DrawPixel(Color.FromArgb(39, 39, 39), 1, 14)
            DrawPixel(Color.FromArgb(39, 39, 39), 14, 14)
            G.DrawLine(New Pen(Color.FromArgb(150, 150, 150)), 2, 1, 13, 1)
            G.DrawString("a", New Font("Webdings", 13), New SolidBrush(Color.FromArgb(12, 12, 12)), New Point(-3, -2))
            G.DrawString("a", New Font("Webdings", 13), Brushes.White, New Point(-3, -3))
        Else
            Dim Gbrush As New LinearGradientBrush(New Rectangle(New Point(1, 1), New Point(14, 14)), Color.FromArgb(74, 74, 74), Color.FromArgb(22, 22, 22), 90.0F)
            G.FillRectangle(Gbrush, New Rectangle(New Point(1, 1), New Point(14, 14)))
            G.DrawLine(Pens.Black, 1, 0, 14, 0)
            G.DrawLine(Pens.Black, 0, 1, 0, 14)
            G.DrawLine(Pens.Black, 15, 1, 15, 14)
            G.DrawLine(Pens.Black, 1, 15, 14, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 0)
            DrawPixel(Color.FromArgb(40, Color.Black), 15, 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 0, 15)
            DrawPixel(Color.FromArgb(85, 85, 85), 1, 1)
            DrawPixel(Color.FromArgb(85, 85, 85), 1, 2)
            DrawPixel(Color.FromArgb(85, 85, 85), 14, 1)
            DrawPixel(Color.FromArgb(85, 85, 85), 14, 2)
            DrawPixel(Color.FromArgb(16, 16, 16), 1, 14)
            DrawPixel(Color.FromArgb(16, 16, 16), 14, 14)
            G.DrawLine(New Pen(Color.FromArgb(110, 110, 110)), 2, 1, 13, 1)
        End If
        G.DrawString(Text, Font, Brushes.Black, 20, 2)
        G.DrawString(Text, Font, Brushes.White, 20, 1)
    End Sub
End Class

Class EvolveRadiobutton
    Inherits ThemeControl154

    Private _Checked As Boolean
    Public Property Checked As Boolean
        Get
            Return _Checked
        End Get
        Set(ByVal V As Boolean)
            _Checked = V
            Invalidate()
        End Set
    End Property

    Protected Overrides Sub OnClick(ByVal e As System.EventArgs)
        MyBase.OnClick(e)
        For Each C As Control In Parent.Controls
            If C.GetType.ToString = Replace(My.Application.Info.ProductName, " ", "_") & ".EvolveRadiobutton" Then
                Dim CC As EvolveRadiobutton
                CC = C
                CC.Checked = False
            Else

            End If
        Next
        _Checked = True
    End Sub

    Protected Overrides Sub ColorHook()

    End Sub

    Protected Overrides Sub OnTextChanged(ByVal e As System.EventArgs)
        MyBase.OnTextChanged(e)
        Dim textSize As Integer
        textSize = Me.CreateGraphics.MeasureString(Text, Font).Width
        Me.Width = 20 + textSize
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(Backcolor)
        G.SmoothingMode = SmoothingMode.AntiAlias

        If _Checked = False Then
            G.FillEllipse(New SolidBrush(Color.Black), 0, 0, 16, 16)
            Dim Gbrush As New LinearGradientBrush(New Rectangle(1, 1, 14, 14), Color.FromArgb(112, 112, 112), Color.FromArgb(25, 25, 25), 90.0F)
            G.FillEllipse(Gbrush, New Rectangle(1, 1, 14, 14))
            Gbrush = New LinearGradientBrush(New Rectangle(2, 2, 12, 12), Color.FromArgb(76, 76, 76), Color.FromArgb(25, 25, 25), 90.0F)
            G.FillEllipse(Gbrush, New Rectangle(2, 2, 12, 12))
        Else
            G.FillEllipse(New SolidBrush(Color.Black), 0, 0, 16, 16)
            Dim Gbrush As New LinearGradientBrush(New Rectangle(1, 1, 14, 14), Color.FromArgb(120, 120, 120), Color.FromArgb(40, 40, 40), 90.0F)
            G.FillEllipse(Gbrush, New Rectangle(1, 1, 14, 14))
            Gbrush = New LinearGradientBrush(New Rectangle(2, 2, 12, 12), Color.FromArgb(100, 100, 100), Color.FromArgb(50, 50, 50), 90.0F)
            G.FillEllipse(Gbrush, New Rectangle(2, 2, 12, 12))
            G.FillEllipse(Brushes.Black, New Rectangle(5, 6, 5, 5))
            G.FillEllipse(Brushes.White, New Rectangle(5, 5, 5, 5))
        End If

        G.DrawString(Text, Font, Brushes.Black, 18, 2)
        G.DrawString(Text, Font, Brushes.White, 18, 1)
    End Sub

    Public Sub New()
        Me.Size = New Point(100, 17)
    End Sub
End Class

Class EvolveControlBox
    Inherits ThemeControl154
    Private _Min As Boolean = True
    Private _Max As Boolean = True
    Private X As Integer

    Protected Overrides Sub ColorHook()
    End Sub

    Public Property MinButton As Boolean
        Get
            Return _Min
        End Get
        Set(value As Boolean)
            _Min = value
            Dim tempwidth As Integer = 40
            If _Min Then tempwidth += 25
            If _Max Then tempwidth += 25
            Me.Width = tempwidth + 1
            Me.Height = 16
            Invalidate()
        End Set
    End Property

    Public Property MaxButton As Boolean
        Get
            Return _Max
        End Get
        Set(value As Boolean)
            _Max = value
            Dim tempwidth As Integer = 40
            If _Min Then tempwidth += 25
            If _Max Then tempwidth += 25
            Me.Width = tempwidth + 1
            Me.Height = 16
            Invalidate()
        End Set
    End Property

    Sub New()
        MyBase.New()
        Size = New Size(92, 16)
        Me.Location = New Point(100, 2)
        Me.Anchor = AnchorStyles.Top Or AnchorStyles.Right
    End Sub

    Protected Overrides Sub OnMove(e As System.EventArgs)
        MyBase.OnMove(e)
        Me.Top = 2
    End Sub


    Protected Overrides Sub OnMouseMove(e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(e)
        X = e.Location.X
        Invalidate()
    End Sub

    Protected Overrides Sub OnClick(e As System.EventArgs)
        MyBase.OnClick(e)
        If _Min And _Max Then
            If X > 0 And X < 25 Then
                FindForm.WindowState = FormWindowState.Minimized
            ElseIf X > 25 And X < 50 Then
                If FindForm.WindowState = FormWindowState.Maximized Then FindForm.WindowState = FormWindowState.Normal Else FindForm.WindowState = FormWindowState.Maximized
            ElseIf X > 50 And X < 90 Then
                FindForm.Close()
            End If
        ElseIf _Min Then
            If X > 0 And X < 25 Then
                FindForm.WindowState = FormWindowState.Minimized
            ElseIf X > 25 And X < 65 Then
                FindForm.Close()
            End If
        ElseIf _Max Then
            If X > 0 And X < 25 Then
                If FindForm.WindowState = FormWindowState.Maximized Then FindForm.WindowState = FormWindowState.Normal Else FindForm.WindowState = FormWindowState.Maximized
            ElseIf X > 25 And X < 65 Then
                FindForm.Close()
            End If
        Else
            If X > 0 And X < 40 Then
                FindForm.Close()
            End If
        End If
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(Color.FromArgb(47, 47, 47))
        Dim cblend As ColorBlend = New ColorBlend(2)
        cblend.Colors(0) = Color.FromArgb(66, 66, 66)
        cblend.Colors(1) = Color.FromArgb(50, 50, 50)
        cblend.Positions(0) = 0
        cblend.Positions(1) = 1
        DrawGradient(cblend, New Rectangle(New Point(0, 0), New Size(Me.Width, Me.Height)))

        If _Min And _Max Then
            If State = MouseState.Over Then
                If X > 0 And X < 25 Then
                    cblend = New ColorBlend(2)
                    cblend.Colors(0) = Color.FromArgb(80, 80, 80)
                    cblend.Colors(1) = Color.FromArgb(60, 60, 60)
                    cblend.Positions(0) = 0
                    cblend.Positions(1) = 1
                    DrawGradient(cblend, New Rectangle(New Point(1, 0), New Size(25, 15)))
                End If
                If X > 25 And X < 50 Then
                    cblend = New ColorBlend(2)
                    cblend.Colors(0) = Color.FromArgb(80, 80, 80)
                    cblend.Colors(1) = Color.FromArgb(60, 60, 60)
                    cblend.Positions(0) = 0
                    cblend.Positions(1) = 1
                    DrawGradient(cblend, New Rectangle(New Point(25, 0), New Size(25, 15)))
                End If
                If X > 50 And X < 90 Then
                    cblend = New ColorBlend(2)
                    cblend.Colors(0) = Color.FromArgb(80, 80, 80)
                    cblend.Colors(1) = Color.FromArgb(60, 60, 60)
                    cblend.Positions(0) = 0
                    cblend.Positions(1) = 1
                    DrawGradient(cblend, New Rectangle(New Point(50, 0), New Size(40, 15)))
                End If
            End If
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(0, 0), New Point(0, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(1, 15), New Point(89, 15))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(25, 0), New Point(25, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(50, 0), New Point(50, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(90, 0), New Point(90, 14))
            DrawPixel(Color.FromArgb(104, 104, 104), 1, 14)
            DrawPixel(Color.FromArgb(104, 104, 104), 89, 14)
            G.DrawString("r", New Font("Marlett", 8), Brushes.White, New Point(63, 2))
            If FindForm.WindowState = FormWindowState.Normal Then
                G.DrawString("1", New Font("Marlett", 8), Brushes.White, New Point(32, 2))
            Else
                G.DrawString("2", New Font("Marlett", 8), Brushes.White, New Point(32, 2))
            End If
            G.DrawString("0", New Font("Marlett", 8), Brushes.White, New Point(6, 2))
        ElseIf _Min Then
            If State = MouseState.Over Then
                If X > 0 And X < 25 Then
                    cblend = New ColorBlend(2)
                    cblend.Colors(0) = Color.FromArgb(80, 80, 80)
                    cblend.Colors(1) = Color.FromArgb(60, 60, 60)
                    cblend.Positions(0) = 0
                    cblend.Positions(1) = 1
                    DrawGradient(cblend, New Rectangle(New Point(1, 0), New Size(25, 15)))
                End If
                If X > 25 And X < 65 Then
                    cblend = New ColorBlend(2)
                    cblend.Colors(0) = Color.FromArgb(80, 80, 80)
                    cblend.Colors(1) = Color.FromArgb(60, 60, 60)
                    cblend.Positions(0) = 0
                    cblend.Positions(1) = 1
                    DrawGradient(cblend, New Rectangle(New Point(25, 0), New Size(40, 15)))
                End If
            End If
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(0, 0), New Point(0, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(25, 0), New Point(25, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(65, 0), New Point(65, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(1, 15), New Point(64, 15))
            DrawPixel(Color.FromArgb(104, 104, 104), 1, 14)
            DrawPixel(Color.FromArgb(104, 104, 104), 64, 14)
            G.DrawString("0", New Font("Marlett", 8), Brushes.White, New Point(6, 2))
            G.DrawString("r", New Font("Marlett", 8), Brushes.White, New Point(38, 2))
        ElseIf _Max Then
            If State = MouseState.Over Then
                If X > 0 And X < 25 Then
                    cblend = New ColorBlend(2)
                    cblend.Colors(0) = Color.FromArgb(80, 80, 80)
                    cblend.Colors(1) = Color.FromArgb(60, 60, 60)
                    cblend.Positions(0) = 0
                    cblend.Positions(1) = 1
                    DrawGradient(cblend, New Rectangle(New Point(1, 0), New Size(25, 15)))
                End If
                If X > 25 And X < 65 Then
                    cblend = New ColorBlend(2)
                    cblend.Colors(0) = Color.FromArgb(80, 80, 80)
                    cblend.Colors(1) = Color.FromArgb(60, 60, 60)
                    cblend.Positions(0) = 0
                    cblend.Positions(1) = 1
                    DrawGradient(cblend, New Rectangle(New Point(25, 0), New Size(40, 15)))
                End If
            End If
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(0, 0), New Point(0, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(25, 0), New Point(25, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(65, 0), New Point(65, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(1, 15), New Point(64, 15))
            DrawPixel(Color.FromArgb(104, 104, 104), 1, 14)
            DrawPixel(Color.FromArgb(104, 104, 104), 64, 14)
            If FindForm.WindowState = FormWindowState.Normal Then
                G.DrawString("1", New Font("Marlett", 8), Brushes.White, New Point(6, 2))
            Else
                G.DrawString("2", New Font("Marlett", 8), Brushes.White, New Point(6, 2))
            End If
            G.DrawString("r", New Font("Marlett", 8), Brushes.White, New Point(38, 2))
        Else
            If State = MouseState.Over Then
                If X > 0 And X < 40 Then
                    cblend = New ColorBlend(2)
                    cblend.Colors(0) = Color.FromArgb(80, 80, 80)
                    cblend.Colors(1) = Color.FromArgb(60, 60, 60)
                    cblend.Positions(0) = 0
                    cblend.Positions(1) = 1
                    DrawGradient(cblend, New Rectangle(New Point(1, 0), New Size(40, 15)))
                End If
            End If
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(0, 0), New Point(0, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(40, 0), New Point(40, 14))
            G.DrawLine(New Pen(Color.FromArgb(104, 104, 104)), New Point(1, 15), New Point(39, 15))
            DrawPixel(Color.FromArgb(104, 104, 104), 1, 14)
            DrawPixel(Color.FromArgb(104, 104, 104), 39, 14)
            G.DrawString("r", New Font("Marlett", 8), Brushes.White, New Point(13, 2))
        End If
    End Sub
End Class

Class EvolveTextBox
    Inherits TextBox

    Protected Overrides Sub WndProc(ByRef m As Message)
        Select Case m.Msg
            Case 15
                Invalidate()
                MyBase.WndProc(m)
                Me.CustomPaint()
                Exit Select
            Case Else
                MyBase.WndProc(m)
                Exit Select
        End Select
    End Sub

    Sub New()
        Font = New Font("Microsoft Sans Serif", 8)
        ForeColor = Color.White
        BackColor = Color.FromArgb(30, 30, 30)
        BorderStyle = Windows.Forms.BorderStyle.FixedSingle
    End Sub

    Private Sub CustomPaint()
        Dim p As New Pen(Color.FromArgb(85, 85, 85))
        CreateGraphics.DrawLine(p, 0, 0, Width, 0)
        CreateGraphics.DrawLine(p, 0, Height - 1, Width, Height - 1)
        CreateGraphics.DrawLine(p, 0, 0, 0, Height - 1)
        CreateGraphics.DrawLine(p, Width - 1, 0, Width - 1, Height - 1)
    End Sub
End Class

Class EvolveMaskedTextBox
    Inherits MaskedTextBox
    Private textBoxGraphics As Graphics
    Protected Overrides Sub WndProc(ByRef m As Message)
        Select Case m.Msg
            Case 15
                Invalidate()
                MyBase.WndProc(m)
                Me.CustomPaint()
                Exit Select
            Case Else
                MyBase.WndProc(m)
                Exit Select
        End Select
    End Sub

    Sub New()
        Font = New Font("Microsoft Sans Serif", 8)
        ForeColor = Color.White
        BackColor = Color.FromArgb(30, 30, 30)
        BorderStyle = Windows.Forms.BorderStyle.FixedSingle
    End Sub

    Private Sub CustomPaint()
        Dim p As New Pen(Color.FromArgb(85, 85, 85))
        CreateGraphics.DrawLine(p, 0, 0, Width, 0)
        CreateGraphics.DrawLine(p, 0, Height - 1, Width, Height - 1)
        CreateGraphics.DrawLine(p, 0, 0, 0, Height - 1)
        CreateGraphics.DrawLine(p, Width - 1, 0, Width - 1, Height - 1)
    End Sub
End Class

Class EvolveGroupBox
    Inherits GroupBox

    Sub New()
        MyBase.New()
        DoubleBuffered = True
        BackColor = Color.FromArgb(47, 47, 47)
    End Sub

    Protected Overrides Sub OnPaint(e As System.Windows.Forms.PaintEventArgs)
        MyBase.OnPaint(e)
        Dim G As Graphics = e.Graphics
        G.Clear(BackColor)
        Dim Gradient As New LinearGradientBrush(New Rectangle(New Point(2, 2), New Size(Width - 4, Me.CreateGraphics.MeasureString(Text, Font).Height + 5)), Color.FromArgb(59, 59, 59), Color.FromArgb(50, 50, 50), 90.0F)
        G.FillRectangle(Gradient, New Rectangle(New Point(2, 2), New Size(Width - 4, Me.CreateGraphics.MeasureString(Text, Font).Height + 5)))
        G.DrawRectangle(New Pen(Color.FromArgb(70, 70, 70)), New Rectangle(New Point(2, 2), New Size(Width - 4, Me.CreateGraphics.MeasureString(Text, Font).Height + 5)))
        G.DrawRectangle(New Pen(Color.FromArgb(30, 30, 30)), New Rectangle(New Point(1, 1), New Size(Width - 2, Height - 2)))
        G.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), 1, Me.CreateGraphics.MeasureString(Text, Font).Height + 8, Width, Me.CreateGraphics.MeasureString(Text, Font).Height + 8)
        G.DrawRectangle(New Pen(Color.FromArgb(70, 70, 70)), New Rectangle(New Point(2, Me.CreateGraphics.MeasureString(Text, Font).Height + 9), New Size(Width - 4, Me.Height - (Me.CreateGraphics.MeasureString(Text, Font).Height + 11))))
        G.DrawString(Text, Font, Brushes.White, 7, 5)
    End Sub

End Class

Class EvolveComboBox
    Inherits ComboBox
    Private X As Integer
    Private Over As Boolean

    Sub New()
        MyBase.New()
        Font = New Font("Microsoft Sans Serif", 9)
        SetStyle(ControlStyles.AllPaintingInWmPaint Or ControlStyles.ResizeRedraw Or ControlStyles.UserPaint Or ControlStyles.DoubleBuffer, True)
        DrawMode = Windows.Forms.DrawMode.OwnerDrawFixed
        ItemHeight = 16
        DropDownStyle = ComboBoxStyle.DropDownList
    End Sub

    Protected Overrides Sub OnMouseMove(e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(e)
        X = e.Location.X
        Invalidate()
    End Sub

    Protected Overrides Sub OnMouseEnter(e As System.EventArgs)
        MyBase.OnMouseEnter(e)
        Over = True
        Invalidate()
    End Sub

    Protected Overrides Sub OnMouseLeave(e As System.EventArgs)
        MyBase.OnMouseEnter(e)
        Over = False
        Invalidate()
    End Sub

    Protected Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        If Not DropDownStyle = ComboBoxStyle.DropDownList Then DropDownStyle = ComboBoxStyle.DropDownList
        Dim B As New Bitmap(Width, Height)
        Dim G As Graphics = Graphics.FromImage(B)

        G.Clear(Color.FromArgb(50, 50, 50))
        Dim GradientBrush As LinearGradientBrush = New LinearGradientBrush(New Rectangle(0, 0, Width, Height), Color.FromArgb(66, 66, 66), Color.FromArgb(55, 55, 55), 90.0F)
        G.FillRectangle(GradientBrush, New Rectangle(0, 0, Width, Height))

        If X >= Width - 20 And Over Then
            GradientBrush = New LinearGradientBrush(New Rectangle(0, 0, Width, Height), Color.FromArgb(77, 77, 77), Color.FromArgb(55, 55, 55), 90.0F)
            G.FillRectangle(GradientBrush, New Rectangle(Width - 22, 2, 20, Height - 4))
        ElseIf X < Width - 20 And Over Then
            GradientBrush = New LinearGradientBrush(New Rectangle(0, 0, Width, Height), Color.FromArgb(77, 77, 77), Color.FromArgb(55, 55, 55), 90.0F)
            G.FillRectangle(GradientBrush, New Rectangle(2, 2, Width - 27, Height - 4))
        End If

        Dim S1 As Integer = G.MeasureString(" ... ", Font).Height
        If SelectedIndex <> -1 Then
            G.DrawString(Items(SelectedIndex), Font, New SolidBrush(Color.White), 4, Height \ 2 - S1 \ 2)
        Else
            If Not Items Is Nothing And Items.Count > 0 Then
                G.DrawString(Items(0), Font, New SolidBrush(Color.White), 4, Height \ 2 - S1 \ 2)
            Else
                G.DrawString(" ... ", Font, New SolidBrush(Color.White), 4, Height \ 2 - S1 \ 2)
            End If
        End If

        Dim p As New Pen(Color.FromArgb(30, 30, 30))

        G.DrawRectangle(p, 0, 0, Width - 1, Height - 1)
        G.DrawRectangle(New Pen(Color.FromArgb(100, 100, 100)), 1, 1, Width - 3, Height - 3)
        G.DrawLine(New Pen(Color.FromArgb(100, 100, 100)), Width - 25, 1, Width - 25, Height - 3)
        G.DrawLine(p, Width - 24, 0, Width - 24, Height)
        G.DrawLine(New Pen(Color.FromArgb(100, 100, 100)), Width - 23, 1, Width - 23, Height - 3)
        G.FillRectangle(New SolidBrush(Color.FromArgb(30, 30, 30)), New Rectangle(New Point(1, 1), New Size(1, 1)))
        G.FillRectangle(New SolidBrush(BackColor), New Rectangle(New Point(0, 0), New Size(1, 1)))
        G.FillRectangle(New SolidBrush(Color.FromArgb(30, 30, 30)), New Rectangle(New Point(Width - 2, 1), New Size(1, 1)))
        G.FillRectangle(New SolidBrush(BackColor), New Rectangle(New Point(Width - 1, 0), New Size(1, 1)))

        G.FillRectangle(New SolidBrush(Color.FromArgb(30, 30, 30)), New Rectangle(New Point(1, Height - 2), New Size(1, 1)))
        G.FillRectangle(New SolidBrush(BackColor), New Rectangle(New Point(0, Height - 1), New Size(1, 1)))
        G.FillRectangle(New SolidBrush(Color.FromArgb(30, 30, 30)), New Rectangle(New Point(Width - 2, Height - 2), New Size(1, 1)))
        G.FillRectangle(New SolidBrush(BackColor), New Rectangle(New Point(Width - 1, Height - 1), New Size(1, 1)))

        G.FillPolygon(Brushes.Black, Triangle(New Point(Width - 14, Height \ 2), New Size(5, 3)))
        G.FillPolygon(Brushes.White, Triangle(New Point(Width - 15, Height \ 2 - 1), New Size(5, 3)))

        e.Graphics.DrawImage(B.Clone, 0, 0)
        G.Dispose() : B.Dispose()
    End Sub

    Protected Overrides Sub OnDrawItem(e As DrawItemEventArgs)
        If e.Index < 0 Then Exit Sub
        Dim rect As New Rectangle()
        rect.X = e.Bounds.X
        rect.Y = e.Bounds.Y
        rect.Width = e.Bounds.Width - 1
        rect.Height = e.Bounds.Height - 1

        e.DrawBackground()
        If e.State = 785 Or e.State = 17 Then
            e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(50, 50, 50)), e.Bounds)
            e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(80, 80, 80)), e.Bounds)
            e.Graphics.DrawString(Me.Items(e.Index).ToString(), e.Font, Brushes.White, e.Bounds.X, e.Bounds.Y + 1)
        Else
            e.Graphics.FillRectangle(New SolidBrush(BackColor), e.Bounds)
            e.Graphics.DrawString(Me.Items(e.Index).ToString(), e.Font, Brushes.White, e.Bounds.X, e.Bounds.Y + 1)
        End If
        MyBase.OnDrawItem(e)
    End Sub

    Public Function Triangle(ByVal Location As Point, ByVal Size As Size) As Point()
        Dim ReturnPoints(0 To 3) As Point
        ReturnPoints(0) = Location
        ReturnPoints(1) = New Point(Location.X + Size.Width, Location.Y)
        ReturnPoints(2) = New Point(Location.X + Size.Width \ 2, Location.Y + Size.Height)
        ReturnPoints(3) = Location

        Return ReturnPoints
    End Function

    Private Sub GhostComboBox_DropDown(sender As Object, e As System.EventArgs) Handles Me.DropDown

    End Sub

    Private Sub GhostComboBox_DropDownClosed(sender As Object, e As System.EventArgs) Handles Me.DropDownClosed
        DropDownStyle = ComboBoxStyle.Simple
        Application.DoEvents()
        DropDownStyle = ComboBoxStyle.DropDownList
    End Sub

    Private Sub GhostCombo_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.TextChanged
        Invalidate()
    End Sub
End Class

Class EvolveTabControl
    Inherits TabControl

    Sub New()
        SetStyle(ControlStyles.AllPaintingInWmPaint Or ControlStyles.ResizeRedraw Or ControlStyles.UserPaint Or ControlStyles.DoubleBuffer, True)
        DoubleBuffered = True
    End Sub
    Protected Overrides Sub CreateHandle()
        MyBase.CreateHandle()
        Alignment = TabAlignment.Top
    End Sub
    Dim C1 As Color = Color.FromArgb(47, 47, 47)
    Dim C2 As Color = Color.FromArgb(30, 30, 30)
    Dim C3 As Color = Color.FromArgb(70, 70, 70)

    Function ToPen(ByVal color As Color) As Pen
        Return New Pen(color)
    End Function

    Function ToBrush(ByVal color As Color) As Brush
        Return New SolidBrush(color)
    End Function

    Protected Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        Dim B As New Bitmap(Width, Height)
        Dim G As Graphics = Graphics.FromImage(B)
        Try : SelectedTab.BackColor = C1 : Catch : End Try
        G.Clear(Parent.BackColor)
        For i = 0 To TabCount - 1
            If Not i = SelectedIndex Then
                Dim x2 As Rectangle = New Rectangle(GetTabRect(i).X, GetTabRect(i).Y + 3, GetTabRect(i).Width + 2, GetTabRect(i).Height)
                Dim G1 As New LinearGradientBrush(New Point(x2.X, x2.Y), New Point(x2.X, x2.Y + x2.Height), Color.FromArgb(50, 50, 50), Color.FromArgb(40, 40, 40))
                G.FillRectangle(G1, x2) : G1.Dispose()
                G.DrawRectangle(ToPen(C3), x2)
                G.DrawRectangle(ToPen(C2), New Rectangle(x2.X + 1, x2.Y + 1, x2.Width - 2, x2.Height))
                G.DrawString(TabPages(i).Text, Font, Brushes.White, x2, New StringFormat With {.LineAlignment = StringAlignment.Near, .Alignment = StringAlignment.Center})
            End If
        Next

        G.FillRectangle(New SolidBrush(Color.FromArgb(47, 47, 47)), 0, ItemSize.Height, Width, Height)
        G.DrawRectangle(ToPen(C2), 0, ItemSize.Height, Width - 1, Height - ItemSize.Height - 1)
        G.DrawRectangle(ToPen(C3), 1, ItemSize.Height + 1, Width - 3, Height - ItemSize.Height - 3)
        If Not SelectedIndex = -1 Then
            Dim x1 As Rectangle = New Rectangle(GetTabRect(SelectedIndex).X - 2, GetTabRect(SelectedIndex).Y, GetTabRect(SelectedIndex).Width + 3, GetTabRect(SelectedIndex).Height)
            Dim GradientBrush As New LinearGradientBrush(New Rectangle(x1.X + 2, x1.Y + 2, x1.Width - 2, x1.Height), Color.FromArgb(66, 66, 66), Color.FromArgb(55, 55, 55), 90.0F)
            G.FillRectangle(New SolidBrush(Color.FromArgb(47, 47, 47)), New Rectangle(x1.X + 2, x1.Y + 2, x1.Width - 2, x1.Height))
            G.DrawLine(ToPen(C2), New Point(x1.X, x1.Y + x1.Height - 2), New Point(x1.X, x1.Y))
            G.DrawLine(ToPen(C2), New Point(x1.X, x1.Y), New Point(x1.X + x1.Width, x1.Y))
            G.DrawLine(ToPen(C2), New Point(x1.X + x1.Width, x1.Y), New Point(x1.X + x1.Width, x1.Y + x1.Height - 2))

            G.DrawLine(ToPen(C3), New Point(x1.X + 1, x1.Y + x1.Height - 1), New Point(x1.X + 1, x1.Y + 1))
            G.DrawLine(ToPen(C3), New Point(x1.X + 1, x1.Y + 1), New Point(x1.X + x1.Width - 1, x1.Y + 1))
            G.DrawLine(ToPen(C3), New Point(x1.X + x1.Width - 1, x1.Y + 1), New Point(x1.X + x1.Width - 1, x1.Y + x1.Height - 1))

            G.DrawString(TabPages(SelectedIndex).Text, Font, Brushes.White, x1, New StringFormat With {.LineAlignment = StringAlignment.Center, .Alignment = StringAlignment.Center})
        End If
        G.DrawLine(New Pen(Color.FromArgb(47, 47, 47)), New Point(0, 1), New Point(0, 2))

        e.Graphics.DrawImage(B.Clone, 0, 0)
        G.Dispose() : B.Dispose()
    End Sub
End Class

Class EvolveListBox
    Inherits ListBox

    Sub New()
        SetStyle(ControlStyles.DoubleBuffer, True)
        Font = New Font("Microsoft Sans Serif", 9)
        BorderStyle = Windows.Forms.BorderStyle.None
        DrawMode = Windows.Forms.DrawMode.OwnerDrawFixed
        ItemHeight = 16
        ForeColor = Color.White
        BackColor = Color.FromArgb(30, 30, 30)
    End Sub

    Protected Overrides Sub WndProc(ByRef m As System.Windows.Forms.Message)
        Select Case m.Msg
            Case 15
                MyBase.WndProc(m)
                CustomPaint()
                Exit Select
            Case Else
                MyBase.WndProc(m)
                Exit Select
        End Select
    End Sub

    Protected Overrides Sub OnDrawItem(e As System.Windows.Forms.DrawItemEventArgs)
        MyBase.OnDrawItem(e)
        e.DrawBackground()
        Dim rect As New Rectangle(New Point(e.Bounds.Left, e.Bounds.Top + 2), New Size(Bounds.Width, 16))
        e.DrawFocusRectangle()
        If InStr(e.State.ToString, "Selected,") > 0 Then
            e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(75, 75, 75)), e.Bounds)
            e.Graphics.DrawString(" " & Items(e.Index).ToString(), Font, New SolidBrush(Color.White), e.Bounds.X, e.Bounds.Y)
        Else
            e.Graphics.DrawString(" " & Items(e.Index).ToString(), Font, New SolidBrush(ForeColor), e.Bounds.X, e.Bounds.Y)
        End If
        CreateGraphics.DrawRectangle(New Pen(Color.FromArgb(70, 70, 70)), New Rectangle(1, 1, Width - 3, Height - 3))
        CreateGraphics.DrawRectangle(New Pen(Color.FromArgb(30, 30, 30)), New Rectangle(0, 0, Width - 1, Height - 1))
    End Sub

    Sub CustomPaint()
        CreateGraphics.DrawRectangle(New Pen(Color.FromArgb(70, 70, 70)), New Rectangle(1, 1, Width - 3, Height - 3))
        CreateGraphics.DrawRectangle(New Pen(Color.FromArgb(30, 30, 30)), New Rectangle(0, 0, Width - 1, Height - 1))
    End Sub

End Class

Class EvolveCheckedListBox
    Inherits CheckedListBox

    Sub New()
        SetStyle(ControlStyles.DoubleBuffer, True)
        Font = New Font("Microsoft Sans Serif", 9)
        BorderStyle = Windows.Forms.BorderStyle.None
        DrawMode = Windows.Forms.DrawMode.OwnerDrawFixed
        ItemHeight = 16
        ForeColor = Color.White
        BackColor = Color.FromArgb(30, 30, 30)
    End Sub

    Protected Overrides Sub WndProc(ByRef m As System.Windows.Forms.Message)
        Select Case m.Msg
            Case 15
                MyBase.WndProc(m)
                CustomPaint()
                Exit Select
            Case Else
                MyBase.WndProc(m)
                Exit Select
        End Select
    End Sub

    Sub DrawPixel(ByVal Color As Color, x As Integer, y As Integer, graphics As Graphics)
        graphics.FillRectangle(New SolidBrush(Color), x, y, 1, 1)
    End Sub

    Protected Overrides Sub OnDrawItem(e As System.Windows.Forms.DrawItemEventArgs)
        On Error GoTo ERRORZ
        MyBase.OnDrawItem(e)
        e.DrawBackground()
        Dim rect As New Rectangle(New Point(e.Bounds.Left, e.Bounds.Top + 2), New Size(Bounds.Width, 16))
        e.DrawFocusRectangle()
        If InStr(e.State.ToString, "Selected,") > 0 Then
            e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(75, 75, 75)), e.Bounds)
            e.Graphics.DrawString("       " & Items(e.Index).ToString(), Font, New SolidBrush(Color.White), e.Bounds.X, e.Bounds.Y)
        Else
            e.Graphics.DrawString("       " & Items(e.Index).ToString(), Font, New SolidBrush(ForeColor), e.Bounds.X, e.Bounds.Y)
        End If
        Dim a As Integer = e.Index
        a = 16 * a
        If GetItemCheckState(e.Index) Then
            Dim G As Graphics = CreateGraphics()
            Dim Gbrush As New LinearGradientBrush(New Rectangle(New Point(6, a + 1), New Size(14, 14)), Color.FromArgb(120, 120, 120), Color.FromArgb(40, 40, 40), 90.0F)
            G.FillRectangle(Gbrush, New Rectangle(New Point(6, a + 1), New Size(14, 14)))
            G.DrawLine(Pens.Black, 6, a, 19, a)
            G.DrawLine(Pens.Black, 5, a + 1, 5, a + 14)
            G.DrawLine(Pens.Black, 20, a + 1, 20, a + 14)
            G.DrawLine(Pens.Black, 6, a + 15, 19, a + 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 20, a, G)
            DrawPixel(Color.FromArgb(40, Color.Black), 5, a, G)
            DrawPixel(Color.FromArgb(40, Color.Black), 20, a + 15, G)
            DrawPixel(Color.FromArgb(40, Color.Black), 5, a + 15, G)
            DrawPixel(Color.FromArgb(120, 120, 120), 6, a + 1, G)
            DrawPixel(Color.FromArgb(120, 120, 120), 6, a + 2, G)
            DrawPixel(Color.FromArgb(120, 120, 120), 19, a + 1, G)
            DrawPixel(Color.FromArgb(120, 120, 120), 19, a + 2, G)
            DrawPixel(Color.FromArgb(39, 39, 39), 6, a + 14, G)
            DrawPixel(Color.FromArgb(39, 39, 39), 19, a + 14, G)
            G.DrawLine(New Pen(Color.FromArgb(150, 150, 150)), 7, a + 1, 18, a + 1)
            G.DrawString("a", New Font("Webdings", 13), New SolidBrush(Color.FromArgb(12, 12, 12)), New Point(2, a - 2))
            G.DrawString("a", New Font("Webdings", 13), Brushes.White, New Point(2, a - 3))
        Else
            Dim G As Graphics = CreateGraphics()
            Dim Gbrush As New LinearGradientBrush(New Rectangle(New Point(6, a + 1), New Size(14, 14)), Color.FromArgb(120, 120, 120), Color.FromArgb(40, 40, 40), 90.0F)
            G.FillRectangle(Gbrush, New Rectangle(New Point(6, a + 1), New Size(14, 14)))
            G.DrawLine(Pens.Black, 6, a, 19, a)
            G.DrawLine(Pens.Black, 5, a + 1, 5, a + 14)
            G.DrawLine(Pens.Black, 20, a + 1, 20, a + 14)
            G.DrawLine(Pens.Black, 6, a + 15, 19, a + 15)
            DrawPixel(Color.FromArgb(40, Color.Black), 20, a, G)
            DrawPixel(Color.FromArgb(40, Color.Black), 5, a, G)
            DrawPixel(Color.FromArgb(40, Color.Black), 20, a + 15, G)
            DrawPixel(Color.FromArgb(40, Color.Black), 5, a + 15, G)
            DrawPixel(Color.FromArgb(120, 120, 120), 6, a + 1, G)
            DrawPixel(Color.FromArgb(120, 120, 120), 6, a + 2, G)
            DrawPixel(Color.FromArgb(120, 120, 120), 19, a + 1, G)
            DrawPixel(Color.FromArgb(120, 120, 120), 19, a + 2, G)
            DrawPixel(Color.FromArgb(39, 39, 39), 6, a + 14, G)
            DrawPixel(Color.FromArgb(39, 39, 39), 19, a + 14, G)
            G.DrawLine(New Pen(Color.FromArgb(150, 150, 150)), 7, a + 1, 18, a + 1)
        End If
        CreateGraphics.DrawRectangle(New Pen(Color.FromArgb(70, 70, 70)), New Rectangle(1, 1, Width - 3, Height - 3))
        CreateGraphics.DrawRectangle(New Pen(Color.FromArgb(30, 30, 30)), New Rectangle(0, 0, Width - 1, Height - 1))
        Exit Sub
ERRORZ:
    End Sub

    Sub CustomPaint()
        CreateGraphics.DrawRectangle(New Pen(Color.FromArgb(70, 70, 70)), New Rectangle(1, 1, Width - 3, Height - 3))
        CreateGraphics.DrawRectangle(New Pen(Color.FromArgb(30, 30, 30)), New Rectangle(0, 0, Width - 1, Height - 1))
    End Sub

End Class

Class EvolveNumericUpDown
    Inherits ThemeControl154
    Private X As Integer
    Private Y As Integer
    Private _Value As Long
    Private _Max As Long
    Private _Min As Long
    Private Typing As Boolean

    Public Property Value As Long
        Get
            Return _Value
        End Get
        Set(V As Long)
            If V <= _Max And V >= _Min Then _Value = V
            Invalidate()
        End Set
    End Property

    Public Property Maximum As Long
        Get
            Return _Max
        End Get
        Set(V As Long)
            If V > _Min Then _Max = V
            If _Value > _Max Then _Value = _Max
            Invalidate()
        End Set
    End Property

    Public Property Minimum As Long
        Get
            Return _Min
        End Get
        Set(V As Long)
            If V < _Max Then _Min = V
            If _Value < _Min Then _Value = _Min
            Invalidate()
        End Set
    End Property

    Protected Overrides Sub OnMouseMove(e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(e)
        X = e.Location.X
        Y = e.Location.Y
        Invalidate()
        If e.X < Width - 23 Then Cursor = Cursors.IBeam Else Cursor = Cursors.Default
    End Sub

    Protected Overrides Sub OnResize(e As System.EventArgs)
        MyBase.OnResize(e)
        Me.Height = 21
    End Sub

    Sub New()
        _Max = 9999999
        _Min = 0
        Cursor = Cursors.IBeam
    End Sub

    Protected Overrides Sub ColorHook()
    End Sub

    Protected Overrides Sub OnMouseDown(e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseClick(e)
        If X > Me.Width - 21 Then
            If Y < 10 Then
                If (Value + 1) <= _Max Then _Value += 1
            Else
                If (Value - 1) >= _Min Then _Value -= 1
            End If
        Else
            Typing = Not Typing
            Focus()
        End If
        Invalidate()
    End Sub

    Protected Overrides Sub OnKeyPress(e As System.Windows.Forms.KeyPressEventArgs)
        MyBase.OnKeyPress(e)
        Try
            If Typing Then _Value = CStr(CStr(_Value) & e.KeyChar.ToString)
        Catch ex As Exception : End Try
    End Sub

    Protected Overrides Sub OnKeyup(e As System.Windows.Forms.KeyEventArgs)
        MyBase.OnKeyUp(e)
        If e.KeyCode = Keys.Up Then
            If (Value + 1) <= _Max Then _Value += 1
            Invalidate()
        ElseIf e.KeyCode = Keys.Down Then
            If (Value - 1) >= _Min Then _Value -= 1
        End If
        Invalidate()
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(Color.FromArgb(50, 50, 50))
        G.DrawString(Value, Font, Brushes.White, New Point(5, 4))
        G.FillRectangle(New SolidBrush(Color.FromArgb(50, 50, 50)), New Rectangle(New Point(Me.Width - 23, 0), New Size(Me.Width - 23, Height)))
        G.FillRectangle(New SolidBrush(Color.FromArgb(70, 70, 70)), New Rectangle(New Point(Width - 21, 0), New Point(3, Height)))
        DrawBorders(New Pen(Color.FromArgb(70, 70, 70)), 1)
        G.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(Width - 20, 0), New Point(Width - 20, Height))
        G.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(Width - 20, 10), New Point(Width, 10))
        G.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(Width - 19, 11), New Point(Width - 1, 11))
        G.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(Width - 19, 9), New Point(Width - 1, 9))
        DrawBorders(New Pen(Color.FromArgb(30, 30, 30)))
        '////UpArrow
        DrawPixel(Color.White, Width - 11, 4)
        DrawPixel(Color.White, Width - 10, 5)
        DrawPixel(Color.White, Width - 11, 5)
        DrawPixel(Color.White, Width - 12, 5)
        DrawPixel(Color.White, Width - 9, 6)
        DrawPixel(Color.White, Width - 10, 6)
        DrawPixel(Color.White, Width - 11, 6)
        DrawPixel(Color.White, Width - 12, 6)
        DrawPixel(Color.White, Width - 13, 6)
        '////DownArrow
        DrawPixel(Color.White, Width - 9, 14)
        DrawPixel(Color.White, Width - 10, 14)
        DrawPixel(Color.White, Width - 11, 14)
        DrawPixel(Color.White, Width - 12, 14)
        DrawPixel(Color.White, Width - 13, 14)
        DrawPixel(Color.White, Width - 10, 15)
        DrawPixel(Color.White, Width - 11, 15)
        DrawPixel(Color.White, Width - 12, 15)
        DrawPixel(Color.White, Width - 11, 16)
        If X > Me.Width - 21 And State = MouseState.Over Then
            If Y < 10 Then
                Dim GradientBrush As New LinearGradientBrush(New Rectangle(New Point(Width - 19, 2), New Size(17, 9)), Color.FromArgb(20, Color.White), Color.FromArgb(0, 0, 0, 0), 90.0F)
                G.FillRectangle(GradientBrush, New Rectangle(New Point(Width - 19, 2), New Size(17, 9)))
            Else
                Dim GradientBrush As New LinearGradientBrush(New Rectangle(New Point(Width - 19, 2), New Size(17, 9)), Color.FromArgb(20, Color.White), Color.FromArgb(0, 0, 0, 0), 90.0F)
                G.FillRectangle(GradientBrush, New Rectangle(New Point(Width - 19, 12), New Size(17, 19)))
            End If
        End If
    End Sub
End Class

Class EvolveMenuStrip
    Inherits MenuStrip
    Dim WithEvents tmr_BottomLine As Windows.Forms.Timer

    Sub New()
        Dock = DockStyle.None
        Renderer = New ToolStripProfessionalRenderer(New EvolveMenuStripColorScheme)
        AutoSize = False
        Location = New Point(3, 26)
        ForeColor = Color.White
        Invalidate()
        tmr_BottomLine = New Timer()
        tmr_BottomLine.Interval = 1
    End Sub

    Protected Overrides Sub OnItemClicked(e As System.Windows.Forms.ToolStripItemClickedEventArgs)
        MyBase.OnItemClicked(e)
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(0, Height - 2), New Point(Width, Height - 2))
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(0, Height - 1), New Point(Width, Height - 1))
        If Not DesignMode Then tmr_BottomLine.Start()
    End Sub

    Protected Overrides Sub OnMouseClick(e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseClick(e)
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(0, Height - 2), New Point(Width, Height - 2))
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(0, Height - 1), New Point(Width, Height - 1))
        If Not DesignMode Then tmr_BottomLine.Start()
    End Sub

    Protected Overrides Sub OnControlAdded(e As System.Windows.Forms.ControlEventArgs)
        MyBase.OnControlAdded(e)
        Invalidate()
    End Sub

    Protected Overrides Sub OnClick(e As System.EventArgs)
        MyBase.OnClick(e)
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(0, Height - 2), New Point(Width, Height - 2))
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(0, Height - 1), New Point(Width, Height - 1))
        If Not DesignMode Then tmr_BottomLine.Start()
    End Sub

    Protected Overrides Sub OnMouseLeave(e As System.EventArgs)
        MyBase.OnMouseLeave(e)
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(0, Height - 2), New Point(Width, Height - 2))
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(0, Height - 1), New Point(Width, Height - 1))
        If Not DesignMode Then tmr_BottomLine.Start()
    End Sub

    Protected Overrides Sub OnMouseMove(mea As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(mea)
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(0, Height - 2), New Point(Width, Height - 2))
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(0, Height - 1), New Point(Width, Height - 1))
        If Not DesignMode Then tmr_BottomLine.Start()
    End Sub

    Protected Overrides Sub OnPaint(e As System.Windows.Forms.PaintEventArgs)
        MyBase.OnPaint(e)
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(0, Height - 2), New Point(Width, Height - 2))
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(0, Height - 1), New Point(Width, Height - 1))
        If Not DesignMode Then tmr_BottomLine.Start()
    End Sub

    Private Sub tmr_BottomLine_Tick(sender As Object, e As System.EventArgs) Handles tmr_BottomLine.Tick
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(30, 30, 30)), New Point(0, Height - 2), New Point(Width, Height - 2))
        CreateGraphics.DrawLine(New Pen(Color.FromArgb(70, 70, 70)), New Point(0, Height - 1), New Point(Width, Height - 1))
    End Sub
End Class

Public Class EvolveMenuStripColorScheme
    Inherits ProfessionalColorTable

    Public Overrides ReadOnly Property ButtonCheckedGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(77, 77, 77)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonCheckedGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonCheckedGradientMiddle() As System.Drawing.Color
        Get
            Return Color.FromArgb(0, 0, 0)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonPressedBorder() As System.Drawing.Color
        Get
            Return Color.FromArgb(70, 70, 70)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonPressedGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(77, 77, 77)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonPressedGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonPressedGradientMiddle() As System.Drawing.Color
        Get
            Return Color.FromArgb(0, 0, 0)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonSelectedBorder() As System.Drawing.Color
        Get
            Return Color.FromArgb(30, 30, 30)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonSelectedGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(77, 77, 77)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonSelectedGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property ButtonSelectedGradientMiddle() As System.Drawing.Color
        Get
            Return Color.FromArgb(0, 0, 0)
        End Get
    End Property

    Public Overrides ReadOnly Property CheckBackground() As System.Drawing.Color
        Get
            Return Color.FromArgb(60, 60, 60)
        End Get
    End Property

    Public Overrides ReadOnly Property CheckPressedBackground() As System.Drawing.Color
        Get
            Return Color.FromArgb(60, 60, 60)
        End Get
    End Property

    Public Overrides ReadOnly Property CheckSelectedBackground() As System.Drawing.Color
        Get
            Return Color.FromArgb(60, 60, 60)
        End Get
    End Property

    Public Overrides ReadOnly Property GripDark() As System.Drawing.Color
        Get
            Return Color.FromArgb(30, 30, 30)
        End Get
    End Property

    Public Overrides ReadOnly Property GripLight() As System.Drawing.Color
        Get
            Return Color.FromArgb(70, 70, 70)
        End Get
    End Property

    Public Overrides ReadOnly Property ImageMarginGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property ImageMarginGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(40, 40, 40)
        End Get
    End Property

    Public Overrides ReadOnly Property ImageMarginGradientMiddle() As System.Drawing.Color
        Get
            Return Color.FromArgb(0, 80, 80, 80)
        End Get
    End Property

    Public Overrides ReadOnly Property ImageMarginRevealedGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(77, 77, 77)
        End Get
    End Property

    Public Overrides ReadOnly Property ImageMarginRevealedGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property ImageMarginRevealedGradientMiddle() As System.Drawing.Color
        Get
            Return Color.FromArgb(80, 80, 80)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuBorder() As System.Drawing.Color
        Get
            Return Color.FromArgb(30, 30, 30)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuItemBorder() As System.Drawing.Color
        Get
            Return Color.FromArgb(70, 70, 70)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuItemPressedGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuItemPressedGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(88, 88, 88)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuItemPressedGradientMiddle() As System.Drawing.Color
        Get
            Return Color.FromArgb(0, 0, 0)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuItemSelected() As System.Drawing.Color
        Get
            Return Color.FromArgb(88, 88, 88)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuItemSelectedGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuItemSelectedGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuStripGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(55, 55, 55)
        End Get
    End Property

    Public Overrides ReadOnly Property MenuStripGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(55, 55, 55)
        End Get
    End Property

    Public Overrides ReadOnly Property OverflowButtonGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(77, 77, 77)
        End Get
    End Property

    Public Overrides ReadOnly Property OverflowButtonGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property OverflowButtonGradientMiddle() As System.Drawing.Color
        Get
            Return Color.FromArgb(0, 0, 0)
        End Get
    End Property

    Public Overrides ReadOnly Property RaftingContainerGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(77, 77, 77)
        End Get
    End Property

    Public Overrides ReadOnly Property RaftingContainerGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(66, 66, 66)
        End Get
    End Property

    Public Overrides ReadOnly Property SeparatorDark() As System.Drawing.Color
        Get
            Return Color.FromArgb(30, 30, 30)
        End Get
    End Property

    Public Overrides ReadOnly Property SeparatorLight() As System.Drawing.Color
        Get
            Return Color.FromArgb(70, 70, 70)
        End Get
    End Property

    Public Overrides ReadOnly Property ToolStripBorder() As System.Drawing.Color
        Get
            Return Color.FromArgb(70, 70, 70)
        End Get
    End Property

    Public Overrides ReadOnly Property ToolStripDropDownBackground() As System.Drawing.Color
        Get
            Return Color.FromArgb(60, 60, 60)
        End Get
    End Property

    Public Overrides ReadOnly Property ToolStripGradientBegin() As System.Drawing.Color
        Get
            Return Color.FromArgb(55, 55, 55)
        End Get
    End Property

    Public Overrides ReadOnly Property ToolStripGradientEnd() As System.Drawing.Color
        Get
            Return Color.FromArgb(55, 55, 55)
        End Get
    End Property

    Public Overrides ReadOnly Property ToolStripGradientMiddle() As System.Drawing.Color
        Get
            Return Color.FromArgb(0, 0, 0)
        End Get
    End Property
End Class
