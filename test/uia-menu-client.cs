using System;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using System.Windows.Automation;
class Client {
 static Process child;
 [StructLayout(LayoutKind.Sequential)] struct Point { public int X,Y; }
 [DllImport("user32.dll")] static extern bool ClientToScreen(IntPtr window,ref Point point);
 static string Property(AutomationElement node,AutomationProperty property) { var value=node.GetCurrentPropertyValue(property,true);return value==AutomationElement.NotSupported?"unsupported":value.ToString(); }

 static void Require(bool ok,string label) { if(!ok) throw new Exception(label); }
 static string Line() { var task=child.StandardOutput.ReadLineAsync(); if(!task.Wait(10000)) throw new Exception("owned provider reply deadline"); var line=task.Result; if(line==null) throw new Exception("provider exited: "+child.ExitCode+" "+child.StandardError.ReadToEnd());return line; }
 static void Command(string text,int expected) { child.StandardInput.WriteLine(text);child.StandardInput.Flush();Require(Line()=="ACK "+expected+" 1","identified provider operation failed"); }
 static string Description(IntPtr hwnd,string name) { var p=Process.Start(new ProcessStartInfo(System.IO.Path.Combine(System.IO.Path.GetDirectoryName(child.StartInfo.FileName),"Inspect.exe"),hwnd.ToInt64()+" "+name) {UseShellExecute=false,RedirectStandardOutput=true,RedirectStandardError=true,CreateNoWindow=true});Require(p.WaitForExit(10000)&&p.ExitCode==0,"native UIA FullDescription query");return p.StandardOutput.ReadToEnd().Trim(); }
 static AutomationElement Named(AutomationElement root,string name) { var found=root.FindFirst(TreeScope.Descendants,new PropertyCondition(AutomationElement.NameProperty,name));Require(found!=null,"missing provider node "+name);return found; }
 static void Retired(AutomationElement node,string label) { try { var name=node.Current.Name;throw new Exception(label+" retained name "+name); } catch(ElementNotAvailableException) {} }
 [MTAThread] static int Main(string[] args) {
  try {
   child=new Process();child.StartInfo=new ProcessStartInfo(args[0]) {UseShellExecute=false,RedirectStandardInput=true,RedirectStandardOutput=true,RedirectStandardError=true,CreateNoWindow=true,WorkingDirectory=System.IO.Path.GetDirectoryName(args[0])};child.Start();
   string[] ready=Line().Split(' ');Require(ready[0]=="READY","provider readiness identity");IntPtr hwnd=new IntPtr(long.Parse(ready[1]));
   var root=AutomationElement.FromHandle(hwnd);Require(root.Current.Name=="Hide","ordinary UIA provider root, not HWND fallback");
   Require(root.Current.ProcessId==child.Id,"owned UIA process identity");
   var item=Named(root,"First");var radio=Named(root,"Current");
   Require(item.Current.ControlType==ControlType.MenuItem && !item.Current.IsEnabled,"menu role/disabled");
   Console.WriteLine("traits item="+item.Current.ControlType.ProgrammaticName+" radio="+radio.Current.ControlType.ProgrammaticName+" itemSelected="+Property(item,SelectionItemPattern.IsSelectedProperty)+" radioSelected="+Property(radio,SelectionItemPattern.IsSelectedProperty)+" radioToggled="+Property(radio,TogglePattern.ToggleStateProperty)+" key="+item.Current.AcceleratorKey);
   Require(radio.Current.ControlType==ControlType.RadioButton,"radio menu role");
   object pattern;Require(!item.TryGetCurrentPattern(InvokePattern.Pattern,out pattern),"read-only menu must not expose Invoke");
   Require(!radio.TryGetCurrentPattern(InvokePattern.Pattern,out pattern),"enabled read-only radio must not expose Invoke");
   Require(Property(radio,SelectionItemPattern.IsSelectedProperty)=="True","radio check representation");
   Require(Description(hwnd,"First")=="Current in editor. Shortcut: Ctrl+O","independent current and shortcut description");
   var rectangle=item.Current.BoundingRectangle;var point=new Point{X=10,Y=20};Require(ClientToScreen(hwnd,ref point),"client bounds conversion");
   Console.WriteLine("bounds="+rectangle+" expected="+point.X+","+point.Y+",170,20");
   Require(Math.Abs(rectangle.X-point.X)<0.1&&Math.Abs(rectangle.Y-point.Y)<0.1&&Math.Abs(rectangle.Width-170)<0.1&&Math.Abs(rectangle.Height-20)<0.1,"client pixel bounds");

   int[] identity=item.GetRuntimeId();Command("rename",1);Require(item.Current.Name=="Renamed","same live node updates");
   Require(identity.SequenceEqual(Named(root,"Renamed").GetRuntimeId()),"live runtime identity");
   Command("clear",2);Retired(item,"hidden item");Retired(radio,"hidden radio");Require(root.FindAll(TreeScope.Children,Condition.TrueCondition).Count==0,"cleared root children");
   Command("reopen",4);var nextRoot=AutomationElement.FromHandle(hwnd);var next=Named(nextRoot,"New attachment");Require(!identity.SequenceEqual(next.GetRuntimeId()),"new node incarnation after hide");
   Command("close",3);Retired(next,"closed adapter item");Command("reopen",4);var finalRoot=AutomationElement.FromHandle(hwnd);var final=Named(finalRoot,"New attachment");Retired(next,"old attachment after reopen");
   Command("invalid",5);Retired(final,"invalid snapshot item");
   child.StandardInput.WriteLine("quit");child.StandardInput.Flush();Require(child.WaitForExit(10000)&&child.ExitCode==0,"owned shutdown");
   Console.WriteLine("UIA ordinary HWND provider discovery, menu state, inert actions, live identity, clear/close/reopen and invalid withdrawal passed; session="+Process.GetCurrentProcess().SessionId);return 0;
  } catch(Exception ex) { Console.Error.WriteLine(ex.ToString());return 1; }
  finally { if(child!=null&&!child.HasExited) {try {child.StandardInput.WriteLine("quit");child.StandardInput.Flush();}catch{} if(!child.WaitForExit(5000))child.Kill();} }
 }
}
