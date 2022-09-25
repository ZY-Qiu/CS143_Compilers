(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

(*
* StackCommand defines basic stack operation, like push, pop and peek
*)

class StackCommand {
   getChar(): String {
      {
         (new IO).out_string("error: getChar() called from base class!\n");
         "";
      }
   };

   getNum(): Int {
      0
   };

   execute(s: Stack): Stack {
      let node: Stack in {
         (new IO).out_string("error: execute called from base class!\n");
         node;
      }
   };
};

class IntCommand inherits StackCommand {
   number: Int;
   
   init(n: Int): SELF_TYPE {
      {
         number <- n;
         self;
      }
   };

   getChar(): String {
      (new A2I).i2a(number)
   };

   getNum(): Int {
      number
   };

   execute(s: Stack): Stack {
      s
   };
};

class PlusCommand inherits StackCommand {
   init():SELF_TYPE {
      self
   };

   getChar(): String {
      "+"
   };

   getNum(): Int {
      {
         (new IO).out_string("error: getNum() called on push command!\n");
         0;
      }
   };

   execute(s: Stack): Stack {
      let node: Stack <- s.pop().pop(), 
         num: IntCommand,
         ret: Stack
      in {
         s <- s.pop();
         num <- (new IntCommand).init(node.peek().getNum() + s.peek().getNum());
         -- cannot use push directly, for the stackTop can be void node
         ret <- (new Stack).init(num, node.pop());
         ret;
      }
   };
};

class SwapCommand inherits StackCommand {
   init():SELF_TYPE {
      self
   };

   getChar(): String {
      "s"
   };

   getNum(): Int {
      {
         (new IO).out_string("error: getNum() called on swap command!\n");
         0;
      }
   };

   execute(s: Stack): Stack {
      let node: Stack <- s.pop().pop() in {
         s <- s.pop();
         s.setNext(node.pop());
         node.setNext(s);
         node;
      }
   };
};

class Stack {
   command: StackCommand;
   next: Stack;

   init(sc: StackCommand, ne: Stack): SELF_TYPE {
      {
         command <- sc;
         next <- ne;
         self;
      }
   };

   push(sc: StackCommand): Stack {
      let top: Stack in {
         top <- (new Stack).init(sc, self);
         top;
      }
   };

   peek(): StackCommand {
      command
   };

   pop(): Stack {
      next
   };

   setNext(n: Stack): Stack {
      {
         next <- n;
      }
   };
};

class Main inherits A2I {
   stackTop: Stack;

   printStack(): Object {
      {
         let node: Stack <- stackTop in {
            while (not (isvoid node)) loop {
               (new IO).out_string(node.peek().getChar());
               (new IO).out_string("\n");
               node <- node.pop();
            }
            pool;
         };
      }
   };

   push(command: StackCommand): StackCommand {
      {
         if(isvoid stackTop) then {
            let nil: Stack in {
               stackTop <- (new Stack).init(command, nil);
            };
         } else {
            stackTop <- stackTop.push(command);
         }
         fi;
         command;
      }
   };

   executeStack(str: String) : Object {
      {
         if(str = "+") then {
            push((new PlusCommand).init());
         }
         else {
            if(str = "s") then {
               push((new SwapCommand).init());
            }
            else {
               if(str = "d") then {
                  printStack();
               }
               else {
                  if(str = "x") then {
                     (new IO).out_string("COOl program successfully executed\n");
                     abort();
                  }
                  else {
                     if(str = "e") then {
                        let node: Stack <- stackTop in {
                           if(not (isvoid node)) then {
                              stackTop <- node.peek().execute(node);
                           }
                           else {
                              0;
                           }
                           fi;
                        };
                     }
                     else {
                        -- int
                        push((new IntCommand).init(a2i(str)));
                     }
                     fi;
                  }
                  fi;
               }
               fi;
            }
            fi;
         }
         fi;
      }
   };
   
   main() : Object {
      let command: String in {
         while (true) loop {
            (new IO).out_string(">");
            command <- (new IO).in_string();
            executeStack(command);
         }
         pool;
      }
   };

};
