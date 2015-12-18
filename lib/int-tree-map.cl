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
};

class IntTreeMapIterator inherits IntMapIterator {
   node : IntTreeMapNode;
   root : IntTreeMapNode;

   init(root_ : IntTreeMapNode) : SELF_TYPE {{
      root <- root_;
      self;
   }};

   minimum(x : IntTreeMapNode) : IntTreeMapNode {{
      let lx : IntTreeMapNode <- x.left() in
         while not isvoid lx loop
            {
               x <- lx;
               lx <- lx.left();
            }
         pool;

      x;
   }};

   successor(x : IntTreeMapNode) : IntTreeMapNode {
      let rx : IntTreeMapNode <- x.right() in
         if not isvoid rx then
            minimum(rx)
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

   next() : Bool {
      if isvoid node then
         if isvoid root then
            false
         else
            {
               node <- minimum(root);
               root <- let void : IntTreeMapNode in void;
               not isvoid node;
            }
         fi
      else
         {
            node <- successor(node);
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

   insertFixup(z : IntTreeMapNode) : Object {{
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
                                       insertFixup(x);
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
                                       insertFixup(x);
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
            insertTreeNode(key).setValue(value);
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
};
