with ZMQ;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Command_Line;
with GNAT.Command_Line;

procedure getinfo is
   use GNAT.Command_Line;
   use Ada.Text_IO;
   command_Name : constant String :=
                    Ada.Directories.Base_Name (Ada.Command_Line.Command_Name);
   procedure help;
   procedure help is
      use ASCII;
   begin
      Put_Line
        (command_Name & " [options]" & LF &
         "Options:" & LF &
         " --binding-version      Print Binding version" & LF &
         " --library-version      Print version of the 0mq library." & LF &
         " -? | -h | --help       Print this text");

   end help;

begin
   loop
      case Getopt ("-binding-version " &
                   "-library-version " &
                   "h ? -help") is  -- Accepts '-a', '-ad', or '-b argument'
         when ASCII.NUL => exit;

         when 'h' | '?' =>
            help;
            return;

         when '-' =>
            if Full_Switch = "-binding-version" then
               Put_Line (ZMQ.image (ZMQ.Binding_Version));
            elsif Full_Switch = "-library-version" then
               Put_Line (ZMQ.image (ZMQ.Library_Version));
            elsif Full_Switch = "-help" then
               help;
               return;
            end if;
         when others =>
            raise Program_Error;         -- cannot occur!
      end case;
   end loop;

   loop
      declare
         S : constant String := Get_Argument (Do_Expansion => True);
      begin
         exit when S'Length = 0;
         Put_Line ("Got " & S);
      end;
   end loop;

exception
   when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);
   when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);
end getinfo;
