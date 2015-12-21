class IntTreeMapNode {
   parent : IntTreeMapNode;
   parent() : IntTreeMapNode { parent };

   setParent(parent_ : IntTreeMapNode) : SELF_TYPE {{
      parent <- parent_;
      self;
   }};

   left : IntTreeMapNode;
   left() : IntTreeMapNode { left };
   setLeft(left_ : IntTreeMapNode) : IntTreeMapNode { left <- left_ };

   createLeft(key : Int) : IntTreeMapNode {
      if isvoid left then
         left <- new IntTreeMapNode.init(key).setParent(self)
      else
         left
      fi
   };

   right : IntTreeMapNode;
   right() : IntTreeMapNode { right };
   setRight(right_ : IntTreeMapNode) : IntTreeMapNode { right <- right_ };

   createRight(key : Int) : IntTreeMapNode {
      if isvoid right then
         right <- new IntTreeMapNode.init(key).setParent(self)
      else
         right
      fi
   };

   -- New nodes are red by default.
   black : Bool;
   black() : Bool { black };
   setBlack(black_ : Bool) : Object { black <- black_ };

   key : Int;
   key() : Int { key };

   value : Object;
   value() : Object { value };
   setValue(value_ : Object) : Object { value <- value_ };

   init(key_ : Int) : SELF_TYPE {{
      key <- key_;
      self;
   }};

   minimum() : IntTreeMapNode {
      let x : IntTreeMapNode <- self,
            lx : IntTreeMapNode <- x.left() in
         {
            while not isvoid lx loop
               {
                  x <- lx;
                  lx <- lx.left();
               }
            pool;

            x;
         }
   };

   successor() : IntTreeMapNode {
      let x : IntTreeMapNode <- self,
            rx : IntTreeMapNode <- x.right() in
         if not isvoid rx then
            rx.minimum()
         else
            let px : IntTreeMapNode <- x.parent() in
               {
                  while if not isvoid px then
                        x = px.right()
                     else false fi
                  loop
                     {
                        x <- px;
                        px <- px.parent();
                     }
                  pool;

                  px;
               }
         fi
   };
};

class IntTreeMapIterator inherits IntMapIterator {
   node : IntTreeMapNode;
   root : IntTreeMapNode;

   init(root_ : IntTreeMapNode) : SELF_TYPE {{
      root <- root_;
      self;
   }};

   next() : Bool {
      if isvoid node then
         if isvoid root then
            false
         else
            {
               node <- root.minimum();
               root <- let void : IntTreeMapNode in void;
               not isvoid node;
            }
         fi
      else
         {
            node <- node.successor();
            not isvoid node;
         }
      fi
   };

   key() : Int {
      node.key()
   };

   value() : Object {
      node.value()
   };
};

class IntTreeMap inherits IntMap {
   root : IntTreeMapNode;

   size : Int;
   size() : Int { size };

   iterator() : IntMapIterator {
      new IntTreeMapIterator.init(root)
   };

   parentOf(node : IntTreeMapNode) : IntTreeMapNode {
      if isvoid node then
         node
      else
         node.parent()
      fi
   };

   leftOf(node : IntTreeMapNode) : IntTreeMapNode {
      if isvoid node then
         node
      else
         node.left()
      fi
   };

   rightOf(node : IntTreeMapNode) : IntTreeMapNode {
      if isvoid node then
         node
      else
         node.right()
      fi
   };

   redOf(node : IntTreeMapNode) : Bool {
      if isvoid node then
         false
      else
         not node.black()
      fi
   };

   blackOf(node : IntTreeMapNode) : Bool {
      if isvoid node then
         true
      else
         node.black()
      fi
   };

   setRedOf(node : IntTreeMapNode) : Object {
      if not isvoid node then
         node.setBlack(false)
      else false fi
   };

   setBlackOf(node : IntTreeMapNode) : Object {
      if not isvoid node then
         node.setBlack(true)
      else false fi
   };

   rotateLeft(x : IntTreeMapNode) : Object {
      if not isvoid x then
         let rx : IntTreeMapNode <- x.right(),
               lrx : IntTreeMapNode <- rx.left(),
               px : IntTreeMapNode <- x.parent() in
            {
               x.setRight(lrx);
               if not isvoid lrx then
                  lrx.setParent(x)
               else false fi;
               rx.setParent(px);
               if isvoid px then
                  root <- rx
               else
                  if px.left() = x then
                     px.setLeft(rx)
                  else
                     px.setRight(rx)
                  fi
               fi;
               rx.setLeft(x);
               x.setParent(rx);
            }
      else false fi
   };

   rotateRight(x : IntTreeMapNode) : Object {
      if not isvoid x then
         let lx : IntTreeMapNode <- x.left(),
               rlx : IntTreeMapNode <- lx.right(),
               px : IntTreeMapNode <- x.parent() in
            {
               x.setLeft(rlx);
               if not isvoid rlx then
                  rlx.setParent(x)
               else false fi;
               lx.setParent(px);
               if isvoid px then
                  root <- lx
               else
                  if px.right() = x then
                     px.setRight(lx)
                  else
                     px.setLeft(lx)
                  fi
               fi;
               lx.setRight(x);
               x.setParent(lx);
            }
      else false fi
   };

   fixupInsert(z : IntTreeMapNode) : Object {{
      while if not isvoid z then
            if not z = root then
               not z.parent().black()
            else false fi
         else false
      fi loop
         let pz : IntTreeMapNode <- z.parent(),
               ppz : IntTreeMapNode <- parentOf(pz),
               lppz : IntTreeMapNode <- leftOf(ppz),
               rppz : IntTreeMapNode <- rightOf(ppz) in
            if pz = lppz then
               if redOf(rppz) then
                  {
                     setBlackOf(pz);
                     setBlackOf(rppz);
                     setRedOf(ppz);
                     z <- ppz;
                  }
               else
                  {
                     if z = rightOf(pz) then
                        {
                           z <- pz;
                           rotateLeft(z);
                        }
                     else false fi;

                     setBlackOf(pz);
                     setRedOf(ppz);
                     rotateRight(ppz);
                  }
               fi
            else
               -- Same as "then" with left/right swapped.
               if redOf(lppz) then
                  {
                     setBlackOf(pz);
                     setBlackOf(lppz);
                     setRedOf(ppz);
                     z <- ppz;
                  }
               else
                  {
                     if z = leftOf(pz) then
                        {
                           z <- pz;
                           rotateRight(z);
                        }
                     else false fi;

                     setBlackOf(pz);
                     setRedOf(ppz);
                     rotateLeft(ppz);
                  }
               fi
            fi
      pool;

      root.setBlack(true);
   }};

   insertTreeNode(k : Int) : IntTreeMapNode {
      let x : IntTreeMapNode <- root in
         if isvoid x then
            {
               size <- 1;
               root <- new IntTreeMapNode.init(k);
            }
         else
            let continue : Bool <- true in
               {
                  while continue loop
                     let kx : Int <- x.key() in
                        if k = kx then
                           continue <- false
                        else
                           if k < kx then
                              let left : IntTreeMapNode <- x.left() in
                                 if isvoid left then
                                    {
                                       x <- x.createLeft(k);
                                       fixupInsert(x);
                                       size <- size + 1;
                                       continue <- false;
                                    }
                                 else
                                    x <- left
                                 fi
                           else
                              let right : IntTreeMapNode <- x.right() in
                                 if isvoid right then
                                    {
                                       x <- x.createRight(k);
                                       fixupInsert(x);
                                       size <- size + 1;
                                       continue <- false;
                                    }
                                 else
                                    x <- right
                                 fi
                           fi
                        fi
                  pool;

                  x;
               }
         fi
   };

   fixupDelete(x : IntTreeMapNode) : Object {{
      while if not x = root then
            x.black()
         else false fi
      loop
         let px : IntTreeMapNode <- x.parent() in
            if x = px.left() then
               let w : IntTreeMapNode <- x.right() in
                  {
                     if redOf(w) then
                        {
                           setBlackOf(w);
                           px.setBlack(false);
                           rotateLeft(px);
                           w <- rightOf(parentOf(x));

                        }
                     else false fi;

                     if if blackOf(leftOf(w)) then
                           blackOf(rightOf(w))
                        else false fi
                     then
                        {
                           setRedOf(w);
                           x <- x.parent();
                        }
                     else
                        {
                           if blackOf(rightOf(w)) then
                              {
                                 setBlackOf(leftOf(w));
                                 setRedOf(w);
                                 rotateRight(w);
                                 w <- rightOf(parentOf(w));
                              }
                           else false fi;

                           let px : IntTreeMapNode <- parentOf(x) in
                              {
                                 if not isvoid w then
                                    w.setBlack(blackOf(px))
                                 else false fi;
                                 setBlackOf(px);
                                 setBlackOf(rightOf(w));
                                 rotateLeft(px);
                              };

                           x <- root;
                        }
                     fi;
                  }
            else
               -- Same as "then" with left/right swapped.
               let w : IntTreeMapNode <- x.left() in
                  {
                     if redOf(w) then
                        {
                           setBlackOf(w);
                           px.setBlack(false);
                           rotateRight(px);
                           w <- leftOf(parentOf(x));

                        }
                     else false fi;

                     if if blackOf(rightOf(w)) then
                           blackOf(leftOf(w))
                        else false fi
                     then
                        {
                           setRedOf(w);
                           x <- x.parent();
                        }
                     else
                        {
                           if blackOf(leftOf(w)) then
                              {
                                 setBlackOf(rightOf(w));
                                 setRedOf(w);
                                 rotateLeft(w);
                                 w <- leftOf(parentOf(w));
                              }
                           else false fi;

                           let px : IntTreeMapNode <- parentOf(x) in
                              {
                                 if not isvoid w then
                                    w.setBlack(blackOf(px))
                                 else false fi;
                                 setBlackOf(px);
                                 setBlackOf(leftOf(w));
                                 rotateRight(px);
                              };

                           x <- root;
                        }
                     fi;
                  }
            fi
      pool;

      x.setBlack(true);
   }};

   deleteTreeNode(z : IntTreeMapNode) : Object {
      let y : IntTreeMapNode <-
               if if isvoid z.left() then
                     true
                  else
                     isvoid z.right()
                  fi
               then
                  z
               else
                  z.successor()
               fi,
            x : IntTreeMapNode <- if not isvoid y.left() then y.left() else y.right() fi in
         {
            if not isvoid x then
               x.setParent(y.parent())
            else false fi;

            let py : IntTreeMapNode <- y.parent() in
               if isvoid py then
                  root <- x
               else
                  if y = py.left() then
                     py.setLeft(x)
                  else
                     py.setRight(x)
                  fi
               fi;

            if not x = z then
               {
                  z.init(y.key());
                  z.setValue(y.value());
               }
            else false fi;

            if y.black() then
               if not isvoid x then
                  fixupDelete(x)
               else false fi
            else false fi;
         }
   };

   getTreeNode(k : Int) : IntTreeMapNode {
      let x : IntTreeMapNode <- root in
         if isvoid x then
            x
         else
            let continue : Bool <- true in
               {
                  while continue loop
                     let kx : Int <- x.key() in
                        if k = kx then
                           continue <- false
                        else
                           {
                              x <- if k < kx then x.left() else x.right() fi;

                              if isvoid x then
                                 continue <- false
                              else false fi;
                           }
                        fi
                  pool;

                  x;
               }
         fi
   };

   putWithInt(key : Int, value : Object) : Object {
      let node : IntTreeMapNode <- insertTreeNode(key),
            oldValue : Object <- node.value() in
         {
            node.setValue(value);
            oldValue;
         }
   };

   getWithInt(key : Int) : Object {
      let node : IntTreeMapNode <- getTreeNode(key) in
         if isvoid node then
            node
         else
            node.value()
         fi
   };

   removeWithInt(key : Int) : Object {
      let node : IntTreeMapNode <- getTreeNode(key) in
         if isvoid node then
            node
         else
            let oldValue : Object <- node.value() in
               {
                  deleteTreeNode(node);
                  size <- size - 1;
                  oldValue;
               }
         fi
   };
};
