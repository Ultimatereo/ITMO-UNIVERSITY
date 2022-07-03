#include <cstdio>
#include <iostream>
#include <fstream>
using namespace std;

struct node {
  int key, prior, size = 1;
  bool rev = false;
  node *l = 0, *r = 0;
  node (int _key) { key = _key, prior = rand(); }
};
int size (node *v) { return v ? v->size : 0; }
void upd (node *v) { v->size =
                        1 +
                        size(v->l) +
                        size(v->r); }
void propagate (node *v) {
  if (v) {
    if (v->rev) {
      swap(v->l, v->r);
      if (v->l)
        v->l->rev ^= true;
      if (v->r)
        v->r->rev ^= true;
    }
    v->rev = false;
  }
}
node* merge (node *l, node *r) {
  propagate(l);
  propagate(r);
  if (!l) return r;
  if (!r) return l;
  if (l->prior > r->prior) {
    l->r = merge(l->r, r);
    upd(l);
    return l;
  }
  else {
    r->l = merge(l, r->l);
    upd(r);
    return r;
  }
}
typedef pair<node*, node*> pairOfTrees;

pairOfTrees split (node *p, int k) {
  propagate(p);
  if (!p) return {nullptr, nullptr};
  if (size(p->l) + 1 <= k) {
    pair q = split(p->r, k - size(p->l) - 1);
    p->r = q.first;
    upd(p);
    return {p, q.second};
  }
  else {
    pair q = split(p->l, k);
    p->l = q.second;
    upd(p);
    return {q.first, p};
  }
}

node* insert (node* root, int x) {
  pair q = split(root, x);
  node *t = new node(x);
  root = merge(q.first, merge(t, q.second));
  return root;
}

node* reverse (node* root, int l, int r) {
  pairOfTrees q1 = split(root, r);
  pairOfTrees q2 = split(q1.first, l - 1);
  if(q2.second) {
    q2.second->rev ^= true;
  }
  root = merge(q2.first, merge(q2.second, q1.second));
  return root;
}

void in_order(struct node* root) {
  if (root) {
    propagate(root);
    in_order(root->l);
    printf("%d ", root->key);
    in_order(root->r);
  }
}
void in_order(struct node* root, ofstream& out) {
  if (root) {
    propagate(root);
    in_order(root->l, out);
//    printf("%d ", root->key);
    out << root->key << " ";
    in_order(root->r, out);
  }
}
int main() {
//    ifstream in("k.txt");
//    ofstream out("ksmart.txt");

  int n, m;
  scanf("%d %d", &n, &m);
//  in >> n >> m;
  node* root = new node(1);
  for (int i = 2; i <= n; i++) {
    root = insert(root, i);
  }

  int l, r;
  for (int i = 0; i < m; i++) {
    scanf("%d %d", &l, &r);
//    in >> l >> r;
    if (r != l) {
      root = reverse(root, l, r);
//      rope<int> cSegment = c.substr(l - 1, r - l + 1);
//      c.erase(l - 1, r - l + 1);
//      rope<int> rcSegment = rc.substr(n - r, r - l + 1);
//      rc.erase(n - r, r - l + 1);
//      c.insert(l - 1, rcSegment);
//      rc.insert(n - r, cSegment);
    }
  }
//  in_order(root, out);
  in_order(root);
}

