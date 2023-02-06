#include <cstdio>
#include <iostream>
#include <fstream>
using namespace std;
class node {
public:
  int key;
  int size;
  node *left, *right;
};
void swap(int a, int b) {
  int temp = a;
  a = b;
  b = temp;
}
node *newNode(int key) {
  node *Node = new node();
  Node->size = 1;
  Node->key = key;
  Node->left = Node->right = nullptr;
  return (Node);
}

node *rightRotate(node *x) {
  node *y = x->left;
  x->left = y->right;
  y->right = x;
  y->size = x->size;
  x->size = (x->right == nullptr ? 0 : x->right->size) +
            (x->left == nullptr ? 0 : x->left->size) + 1;
  return y;
}

node *leftRotate(node *x) {
  node *y = x->right;
  x->right = y->left;
  y->left = x;
  y->size = x->size;
  x->size = (x->right == nullptr ? 0 : x->right->size) +
            (x->left == nullptr ? 0 : x->left->size) + 1;
  return y;
}

node *splay(node *root, int key) {
  if (root == nullptr || root->key == key) {
    return root;
  }
  if (root->key > key) {

    if (root->left == nullptr) {
      return root;
    }
    // Zig-Zig (Left Left)
    if (root->left->key > key) {
      root->left->left = splay(root->left->left, key);
      root = rightRotate(root);
      // Zig-Zag (Left Right)
    } else if (root->left->key < key) {
      root->left->right = splay(root->left->right, key);
      if (root->left->right != nullptr)
        root->left = leftRotate(root->left);
    }
    return (root->left == nullptr) ? root : rightRotate(root);
  } else {
    // Key is not in tree, we are done
    if (root->right == nullptr) {
      return root;
    }
    // Zig-Zag (Right Left)
    if (root->right->key > key) {
      // Bring the key as root of right-left
      root->right->left = splay(root->right->left, key);
      if (root->right->left != nullptr) {
        root->right = rightRotate(root->right);
      }
    } else if (root->right->key < key) {
      root->right->right = splay(root->right->right, key);
      root = leftRotate(root);
    }
    return (root->right == nullptr) ? root : leftRotate(root);
  }
}
node *insert(node *root, int k) {
  if (root == nullptr) {
    return newNode(k);
  }
  root = splay(root, k);
  if (root->key == k) {
    return root;
  }
  node *newnode = newNode(k);
  if (root->key > k) {
    newnode->right = root;
    newnode->left = root->left;
    root->left = nullptr;
  } else {
    newnode->left = root;
    newnode->right = root->right;
    root->right = nullptr;
  }
  root->size = (root->right == nullptr ? 0 : root->right->size) +
            (root->left == nullptr ? 0 : root->left->size) + 1;
  newnode->size = (newnode->right == nullptr ? 0 : newnode->right->size) +
            (newnode->left == nullptr ? 0 : newnode->left->size) + 1;
  return newnode;
}
node *merge(node *tree1, node *tree2) {
  node* x = tree1;
  while (x->right != nullptr) {
    x = x->right;
  }
  x = splay(tree1, x->key);
  x->right = tree2;
  x->size = (x->right == nullptr ? 0 : x->right->size) +
            (x->left == nullptr ? 0 : x->left->size) + 1;
  return x;
}

node *remove(node *root, int k) {
  root = splay(root, k);
  if (root->left == nullptr) {
    return root->right;
  }
  if (root->right == nullptr) {
    return root->left;
  }
  root = merge(root->left, root->right);
  return root;
}

int findKthMax(node *root, int k) {
  node* temp = root;
  while (temp != nullptr) {
    int s = (temp->right == nullptr ? 0 : temp->right->size);
    if (s + 1 == k) {
      return temp->key;
    }
    if (k > s + 1) {
      k -= s + 1;
      temp = temp->left;
    } else {
      temp = temp->right;
    }
  }
  return -1;
}
int main() {
  int n, k, x;
  node *root = nullptr;
//  ifstream in("k.txt");
//  ofstream out("ksmart.txt");
//  in >> n;
  scanf("%d", &n);
  for (int i = 0; i < n; i++) {
    scanf("%d %d", &k, &x);
//    in >> k >> x;
    switch (k) {
    case 1 :
      if (root == nullptr) {
        root = newNode(x);
      } else {
        root = insert(root, x);
      }
      break;
    case -1 :
      root = remove(root, x);
      break;
    case 0 :
      printf("%d\n", findKthMax(root, x));
//      out << findKthMax(root, x) << endl;
      break;
    }
  }
//  in.close();
//  out.close();
}

