Imports TIS.Imaging
Imports System

Public Class Form1
    Dim VideoHasStarted As Boolean
    Dim StartToggle As Integer = 0
    Dim ZoomOutValue As Decimal = 1.2
    Dim TriggerToggle As Integer = 0
    Dim ImgSaveToggle As Integer = 0
    Dim BitToggle As Integer = 0
    Dim TiffToggle As Integer = 0
    Dim impath_hi As String
    Dim impath_lo As String
    Dim imFullpath_hi As String = "C:\"
    Dim imFullpath_lo As String = "C:\"
    Dim IncrementLow As Double = 0.0
    Dim ImageCount As Integer = 0
    Dim SequenceCount As Integer = 0
    Dim FrameCount As Integer = 0
    Dim ExposureProperty As VCDRangeProperty
    Dim ExposureAuto As VCDSwitchProperty
    Dim ExposureMax As VCDRangeProperty
    Dim ExposureMaxAuto As VCDSwitchProperty
    Dim ImageOdd As Integer = 0
    Dim ImageEven As Integer = 0
    Dim ExpValue As Double = 0.1
    Dim ExpSliderValue As Double = 0.1
    Private _imageSink As TIS.Imaging.FrameSnapSink
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        On Error GoTo err_Form_Load
        If Not IcImagingControl2.DeviceValid Then
            IcImagingControl2.ShowDeviceSettingsDialog()

            If Not IcImagingControl2.DeviceValid Then
                MsgBox("No device was selected.")

                Exit Sub
            End If
        End If
        IcImagingControl2.LiveDisplay = True
        IcImagingControl2.LiveCaptureLastImage = False
        IcImagingControl2.LiveCaptureContinuous = True


        IcImagingControl2.Width = IcImagingControl2.ImageWidth
        IcImagingControl2.Height = IcImagingControl2.ImageHeight
        'IcImagingControl1.MemoryCurrentGrabberColorformat = ICImagingControlColorformats.ICRGB64
        IcImagingControl2.MemoryCurrentGrabberColorformat = ICImagingControlColorformats.ICRGB32

        'use this for tiff images
        '_imageSink = New TIS.Imaging.FrameSnapSink(TIS.Imaging.MediaSubtypes.RGB64)
        'IcImagingControl1.Sink = _imageSink
        'IcImagingControl1.ImageAvailableExecutionMode = 1

        Exit Sub

err_Form_Load:
        MsgBox(Err.Description)
    End Sub
    Private Sub ShowBuffer(buffer As IFrameQueueBuffer)
        Try
            IcImagingControl2.DisplayImageBuffer(buffer)
        Catch
            MessageBox.Show("snap image failed, timeout occurred.")
        End Try
    End Sub






    Private Sub IcImagingControl1_Load(sender As Object, e As EventArgs) Handles IcImagingControl2.Load

    End Sub

    Private Sub Start_Click(sender As Object, e As EventArgs) Handles Start.Click
        If StartToggle = 0 Then
            Start.BackColor = Color.Red
            StartToggle = 1
            IcImagingControl2.LiveStart()
            VideoHasStarted = True
        Else
            Start.BackColor = Color.White
            StartToggle = 0
            IcImagingControl2.LiveStop()
            VideoHasStarted = False
        End If
    End Sub

    Private Sub zoomout_click(sender As Object, e As EventArgs) Handles ZoomOut.Click
        IcImagingControl2.LiveDisplayDefault = 0
        IcImagingControl2.LiveDisplayZoomFactor = IcImagingControl2.LiveDisplayZoomFactor / ZoomOutValue
    End Sub

    Private Sub zoomin_click(sender As Object, e As EventArgs) Handles ZoomIn.Click
        IcImagingControl2.LiveDisplayDefault = 0
        IcImagingControl2.LiveDisplayZoomFactor = IcImagingControl2.LiveDisplayZoomFactor * ZoomOutValue
    End Sub

    Private Sub settings_click(sender As Object, e As EventArgs) Handles Settings.Click
        IcImagingControl2.ShowPropertyDialog()
    End Sub

    Private Sub Trigger_click(sender As Object, e As EventArgs) Handles Trigger.Click
        If TriggerToggle = 0 Then
            Trigger.BackColor = Color.Red
            TriggerToggle = 1
            IcImagingControl2.DeviceTrigger = True
        Else
            Trigger.BackColor = Color.White
            TriggerToggle = 0
            IcImagingControl2.DeviceTrigger = False
        End If
    End Sub

    Private Sub imgsave_click(sender As Object, e As EventArgs) Handles ImgSave.Click
        If ImgSaveToggle = 0 Then
            ImgSave.BackColor = Color.Red
            ImgSaveToggle = 1
        Else
            ImgSave.BackColor = Color.White
            ImgSaveToggle = 0
        End If

    End Sub

    Private Sub path1_click(sender As Object, e As EventArgs) Handles Path1.Click
        If impath_hi = "" Then
            Dim dialog As New FolderBrowserDialog()
            dialog.RootFolder = Environment.SpecialFolder.Desktop
            dialog.SelectedPath = "c:\"
            dialog.Description = "select application configuration files path"
            If dialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
                impath_hi = dialog.SelectedPath
            End If
            'my.computer.filesystem.writealltext(impath & "apppath.txt", impath, false)
            Debug.Print(impath_hi)
        End If
    End Sub

    Private Sub path2_click(sender As Object, e As EventArgs) Handles Path2.Click
        If impath_lo = "" Then
            Dim dialog As New FolderBrowserDialog()
            dialog.RootFolder = Environment.SpecialFolder.Desktop
            dialog.SelectedPath = "c:\"
            dialog.Description = "select application configeration files path"
            If dialog.ShowDialog() = Windows.Forms.DialogResult.OK Then
                impath_lo = dialog.SelectedPath
            End If
            'my.computer.filesystem.writealltext(impath & "apppath.txt", impath, false)
            Debug.Print(impath_lo)
        End If
    End Sub

    Private Sub ExpDisplay_click(sender As Object, e As EventArgs) Handles ExpDisplay.Click
        MessageBox.Show("Exposure=" + Str(ExpValue))
    End Sub

    Private Sub icimagingcontrol1_imageavailable(sender As Object, e As ICImagingControl.ImageAvailableEventArgs) Handles IcImagingControl2.ImageAvailable

        On Error GoTo err_imageavailable_handler

        Dim CurrentBuffer As ImageBuffer
        Dim DisplayBuffer As ImageBuffer
        Dim bufferNumber As Integer = IcImagingControl2.ImageBuffers.Count
        CurrentBuffer = IcImagingControl2.ImageActiveBuffer

        ExposureProperty = IcImagingControl2.VCDPropertyItems.FindInterface(VCDGUIDs.VCDID_Exposure, VCDGUIDs.VCDElement_Value, VCDGUIDs.VCDInterface_Range)
        ExposureAuto = IcImagingControl2.VCDPropertyItems.FindInterface(VCDGUIDs.VCDID_Exposure, VCDGUIDs.VCDElement_Auto, VCDGUIDs.VCDInterface_Switch)
        ExposureMax = IcImagingControl2.VCDPropertyItems.FindInterface(VCDGUIDs.VCDID_Exposure, VCDGUIDs.VCDElement_AutoMaxValue)
        ExposureMaxAuto = IcImagingControl2.VCDPropertyItems.FindInterface(VCDGUIDs.VCDID_Exposure, VCDGUIDs.VCDElement_AutoMaxValueAuto, VCDGUIDs.VCDInterface_Switch)


        If ImgSaveToggle = 1 Then
            FrameCount = FrameCount + 1
            ImageCount = ImageCount + 1
            SequenceCount = SequenceCount + 1
            Debug.Print(ImageCount)
            If SequenceCount = 5 Then
                SequenceCount = 1
            End If
            If ImageCount > 0 Then
                If SequenceCount = 2 Then
                    'Threading.Thread.Sleep(50)
                    ExposureAuto.Switch = False
                    'ExposureProperty.Value = ExposureProperty.Value - IncrementLow
                    ExpValue = Exposure_Abs(IcImagingControl2)
                    ExpValue = ExpValue / (2 ^ IncrementLow)
                    Exposure_Abs(IcImagingControl2, ExpValue)
                    ImageOdd = ImageOdd + 1
                    impath_lo = impath_lo.Trim(vbNullChar)
                    If TiffToggle = 1 Then
                        imFullpath_hi = impath_hi + "\" + Str(ImageOdd) + ".tiff"
                    Else
                        imFullpath_hi = impath_hi + "\" + Str(ImageOdd) + ".jpg"
                    End If
                    imFullpath_hi = imFullpath_hi.Replace(" ", "")
                    If TiffToggle = 1 Then
                        CurrentBuffer.SaveAsTiff(imFullpath_hi)
                    Else
                        CurrentBuffer.SaveAsJpeg(imFullpath_hi, 100)
                    End If
                End If
                If SequenceCount = 4 Then
                    'Threading.Thread.Sleep(50)
                    'ExposureProperty.Value = ExposureProperty.Value + IncrementLow
                    ExpValue = Exposure_Abs(IcImagingControl2)
                    ExpValue = ExpValue * (2 ^ IncrementLow)
                    Exposure_Abs(IcImagingControl2, ExpValue)
                    ExposureAuto.Switch = True
                    ImageEven = ImageEven + 1
                    impath_hi = impath_hi.Trim(vbNullChar)
                    If TiffToggle = 1 Then
                        imFullpath_lo = impath_lo + "\" + Str(ImageEven) + ".tiff"
                    Else
                        imFullpath_lo = impath_lo + "\" + Str(ImageEven) + ".jpg"
                    End If
                    imFullpath_lo = imFullpath_lo.Replace(" ", "")
                    If TiffToggle = 1 Then
                        CurrentBuffer.SaveAsTiff(imFullpath_lo)
                    Else
                        CurrentBuffer.SaveAsJpeg(imFullpath_lo, 100)
                    End If
                End If
                'Debug.Print("image count=" + CStr(ImageCount))
                'Debug.Print("imageodd=" + Str(ImageOdd))
                'Debug.Print("imageeven=" + Str(ImageEven))
                'Debug.Print(imFullpath_hi)
                'Debug.Print(imFullpath_lo)
            End If
        End If
err_imageavailable_handler:
        Debug.Print(Err.Description)
    End Sub

    Private Sub SaveConf_Click(sender As Object, e As EventArgs) Handles SaveConf.Click
        IcImagingControl2.SaveDeviceStateToFile("device_state.txt")
    End Sub

    Private Sub LoadConf_Click(sender As Object, e As EventArgs) Handles LoadConf.Click
        IcImagingControl2.LoadDeviceStateFromFile("device_state.txt", True)
    End Sub

    Private Sub Bit64_Click(sender As Object, e As EventArgs) Handles Bit64.Click
        If BitToggle = 0 Then
            Bit64.BackColor = Color.Red
            BitToggle = 1
            IcImagingControl2.MemoryCurrentGrabberColorformat = ICImagingControlColorformats.ICRGB64
        Else
            Bit64.BackColor = Color.White
            BitToggle = 0
            IcImagingControl2.MemoryCurrentGrabberColorformat = ICImagingControlColorformats.ICRGB24
        End If
    End Sub

    Private Sub SaveTiff_Click(sender As Object, e As EventArgs) Handles SaveTiff.Click
        If TiffToggle = 0 Then
            SaveTiff.BackColor = Color.Red
            TiffToggle = 1
        Else
            SaveTiff.BackColor = Color.White
            TiffToggle = 0
        End If
    End Sub
    Private Sub NumericUpDown1_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumericUpDown1.ValueChanged
        'NumericUpDown valu will show in TextBox
        IncrementLow = NumericUpDown1.Value / 10
    End Sub

    Private Sub LoadExp_Click(sender As Object, e As EventArgs) Handles LoadExp.Click
        ExpValue = Exposure_Abs(IcImagingControl2)
    End Sub

    Public Sub Exposure_Abs(ic As TIS.Imaging.ICImagingControl, Value As Double)
        Dim DevProperty As VCDAbsoluteValueProperty
        If Not ic.DeviceValid Then
            Throw New System.Exception("Exposure : No device selected")
        End If
        DevProperty = ic.VCDPropertyItems.FindInterface(New Guid("90d5702e-e43b-4366-aaeb-7a7a10b448b4"), New Guid("b57d3000-0ac6-4819-a609-272a33140aca"), VCDGUIDs.VCDInterface_AbsoluteValue)
        If Not DevProperty Is Nothing Then
            If DevProperty.ReadOnly Then
                Throw New System.Exception("Exposure DevProperty is read only.")
            Else
                If DevProperty.RangeMin <= Value And DevProperty.RangeMax >= Value Then
                    DevProperty.Value = Value
                Else
                    Throw New System.Exception(System.String.Format("Exposure : Value {0} is not in range {1} - {2}.", Value, DevProperty.RangeMin, DevProperty.RangeMax))
                End If
            End If
        Else
            Throw New System.Exception("Exposure Property is not supported by current device.")
        End If
    End Sub
    Public Function Exposure_Abs(ic As TIS.Imaging.ICImagingControl) As Double
        Dim DevProperty As VCDAbsoluteValueProperty
        If Not ic.DeviceValid Then
            Throw New System.Exception("Exposure : No device selected")
        End If

        DevProperty = ic.VCDPropertyItems.FindInterface(New Guid("90d5702e-e43b-4366-aaeb-7a7a10b448b4"), New Guid("b57d3000-0ac6-4819-a609-272a33140aca"), VCDGUIDs.VCDInterface_AbsoluteValue)

        If Not DevProperty Is Nothing Then
            Return DevProperty.Value
        Else
            Throw New System.Exception("Exposure Property is not supported by current device.")
        End If
        Return 0
    End Function
End Class
