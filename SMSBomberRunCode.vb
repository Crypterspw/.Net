Dim num As Integer = 0
        For intLoopIndex = 0 To Convert.ToInt32(TextBox5.Text)
            Dim mail As New MailMessage()
            Dim SmtpServer As New SmtpClient
            SmtpServer.Credentials = New Net.NetworkCredential(TextBox3.Text, TextBox4.Text) ' I've added the option to add your own email credentials via textboxes, although you can just add them here, and you don't have to enter it later. Whatever you wish.
            SmtpServer.Port = 587
            SmtpServer.Host = "smtp.gmail.com" 'Google mail's smtp server, you can change it over to hotmail, or whatever the hell you want, Google is one of the more commonly known ones.
            SmtpServer.EnableSsl = True
            If ComboBox1.SelectedItem = "AT&T" Then
                mail.To.Add(TextBox1.Text + "@txt.att.net")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Movistar" Then
                mail.To.Add(TextBox1.Text + "@movistar.com.co")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "MTN" Then
                mail.To.Add(TextBox1.Text + "@sms.co.za")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "MTS" Then
                mail.To.Add(TextBox1.Text + "@text.mtsmobility.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Nextel (United States)" Then
                mail.To.Add(TextBox1.Text + "@messaging.nextel.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Nextel (Argentina)" Then
                mail.To.Add(TextBox1.Text + "@nextel.net.ar")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Orange Polska" Then
                mail.To.Add(TextBox1.Text + "@orange.pl")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Personal" Then
                mail.To.Add(TextBox1.Text + "@alertas.personal.com.ar")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Plus GSM" Then
                mail.To.Add(TextBox1.Text + "@text.plusgsm.pl")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "President’s Choice" Then
                mail.To.Add(TextBox1.Text + "@txt.bell.ca")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Qwest" Then
                mail.To.Add(TextBox1.Text + "@qwestmp.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Rogers" Then
                mail.To.Add(TextBox1.Text + "@pcs.rogers.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "SL Interactive" Then
                mail.To.Add(TextBox1.Text + "@slinteractive.com.au")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Sasktel" Then
                mail.To.Add(TextBox1.Text + "@sms.sasktel.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Setar Mobile email (Aruba)" Then
                mail.To.Add(TextBox1.Text + "@mas.aw")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Suncom" Then
                mail.To.Add(TextBox1.Text + "@tms.suncom.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "T-Mobile (Austria)" Then
                mail.To.Add(TextBox1.Text + "@sms.t-mobile.at")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "T-Mobile (UK)" Then
                mail.To.Add(TextBox1.Text + "@t-mobile.uk.net")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Telus Mobility" Then
                mail.To.Add(TextBox1.Text + "@msg.telus.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Thumb Cellular" Then
                mail.To.Add(TextBox1.Text + "@sms.thumbcellular.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Tigo (Formerly Ola)" Then
                mail.To.Add(TextBox1.Text + "@sms.tigo.com.co")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Tracfone (prepaid)" Then
                mail.To.Add(TextBox1.Text + "@mmst5.tracfone.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Unicel" Then
                mail.To.Add(TextBox1.Text + "@utext.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Virgin Mobile (Canada)" Then
                mail.To.Add(TextBox1.Text + "@vmobile.ca")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Vodacom (South Africa)" Then
                mail.To.Add(TextBox1.Text + "@voda.co.za")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Vodafone (Italy)" Then
                mail.To.Add(TextBox1.Text + "@sms.vodafone.it")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "YCC" Then
                mail.To.Add(TextBox1.Text + "@sms.ycc.ru")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "MobiPCS (Hawaii only)" Then
                mail.To.Add(TextBox1.Text + "@mobipcs.net")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Claro (Brasil)" Then
                mail.To.Add(TextBox1.Text + "@clarotorpedo.com.br")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Claro (Nicaragua)" Then
                mail.To.Add(TextBox1.Text + "@ideasclaro-ca.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Comcel" Then
                mail.To.Add(TextBox1.Text + "@comcel.com.co")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Cricket" Then
                mail.To.Add(TextBox1.Text + "@sms.mycricket.com (SMS)")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Cricket" Then
                mail.To.Add(TextBox1.Text + "@sms.mycricket.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Globalstar (satellite)" Then
                mail.To.Add(TextBox1.Text + "@msg.globalstarusa.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Helio" Then
                mail.To.Add(TextBox1.Text + "@messaging.sprintpcs.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Illinois Valley Cellular" Then
                mail.To.Add(TextBox1.Text + "@ivctext.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Iridium (satellite)" Then
                mail.To.Add(TextBox1.Text + "@msg.iridium.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Iusacell" Then
                mail.To.Add(TextBox1.Text + "@rek2.com.mx")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "i wireless" Then
                mail.To.Add(TextBox1.Text + "@iwspcs.net")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Koodo Mobile" Then
                mail.To.Add(TextBox1.Text + "@msg.koodomobile.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Airtel (Karnataka, India)" Then
                mail.To.Add(TextBox1.Text + "@airtelkk.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Airtel Wireless (Montana, USA)" Then
                mail.To.Add(TextBox1.Text + "@sms.airtelmontana.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "CTI" Then
                mail.To.Add(TextBox1.Text + "@sms.ctimovil.com.ar")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Emtel" Then
                mail.To.Add(TextBox1.Text + "@emtelworld.net")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Fido" Then
                mail.To.Add(TextBox1.Text + "@fido.ca")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "General Communications Inc." Then
                mail.To.Add(TextBox1.Text + "@msg.gci.net")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Bell Mobility & Solo Mobile" Then
                mail.To.Add(TextBox1.Text + "@txt.bell.ca")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "BPL Mobile" Then
                mail.To.Add(TextBox1.Text + "@bplmobile.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Cellular One" Then
                mail.To.Add(TextBox1.Text + "@mobile.celloneusa.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Cingular (Postpaid)" Then
                mail.To.Add(TextBox1.Text + "@cingularme.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Centennial Wireless" Then
                mail.To.Add(TextBox1.Text + "@cwemail.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Cingular (GoPhone prepaid)" Then
                mail.To.Add(TextBox1.Text + "@cingularme.com (SMS)")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Cingular (GoPhone prepaid)" Then
                mail.To.Add(TextBox1.Text + "@cingularme.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Alaska Communications Systems" Then
                mail.To.Add(TextBox1.Text + "@msg.acsalaska.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Aql" Then
                mail.To.Add(TextBox1.Text + "@text.aql.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "AT&T Enterprise Paging" Then
                mail.To.Add(TextBox1.Text + "@page.att.net")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "BigRedGiant Mobile Solutions" Then
                mail.To.Add(TextBox1.Text + "@tachyonsms.co.uk")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "T-Mobile USA" Then
                mail.To.Add(TextBox1.Text + "@tmomail.net")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Nextel" Then
                mail.To.Add(TextBox1.Text + "@messaging.nextel.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Sprint PCS" Then
                mail.To.Add(TextBox1.Text + "@messaging.sprintpcs.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "T-Mobile (Germany)" Then
                mail.To.Add(TextBox1.Text + "@t-d1-sms.de")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Verizon" Then
                mail.To.Add(TextBox1.Text + "@vtext.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Boost (U.S)" Then
                mail.To.Add(TextBox1.Text + "@myboostmobile.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Virgin Mobile (USA)" Then
                mail.To.Add(TextBox1.Text + "@vmobl.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "LMT" Then
                mail.To.Add(TextBox1.Text + "@sms.lmt.lv")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Meteor" Then
                mail.To.Add(TextBox1.Text + "@sms.mymeteor.ie")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Mero Mobile" Then
                mail.To.Add(TextBox1.Text + "@sms.spicenepal.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "MetroPCS" Then
                mail.To.Add(TextBox1.Text + "@mymetropcs.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Movicom" Then
                mail.To.Add(TextBox1.Text + "@sms.movistar.net.ar")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "Mobitel" Then
                mail.To.Add(TextBox1.Text + "@sms.mobitel.lk")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            ElseIf ComboBox1.SelectedItem = "7-11 Speakout" Then
                mail.To.Add(TextBox1.Text + "@cingularme.com")
                mail.From = New MailAddress(TextBox3.Text)
                mail.Subject = TextBox2.Text
                mail.Body = RichTextBox1.Text
                SmtpServer.Send(mail)
            End If
            num = num + 1
            Label1.Text = num
        Next intLoopIndex
