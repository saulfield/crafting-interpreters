/*
Solution for chapter 1, exercise 3 (https://craftinginterpreters.com/introduction.html#challenges)

"To get some practice with pointers, define a doubly linked list of heap-allocated strings.
Write functions to insert, find, and delete items from it. Test them."

To run:
clang list.c -o list.elf && ./list.elf

Output:
[]
[A,BB,CCC]
[A]
[A,CCC,BB]
1
-1
*/

#include <assert.h> // for assert
#include <stdio.h>  // for printf
#include <string.h> // for strcmp

typedef struct Node {
    struct Node *prev;
    struct Node *next;
    const char *str;
} Node;

typedef struct DoublyLinkedList {
    Node *head;
    Node *tail;
} DoublyLinkedList;

void insert_end(DoublyLinkedList *list, Node *node) {
    if (list->tail == NULL) {
        assert(list->head == NULL);
        list->head = node;
    } else {
        list->tail->next = node;
        node->prev = list->tail;
    }
    list->tail = node;
}

void delete_node(DoublyLinkedList *list, Node *node) {
    if (node == list->head) {
        list->head = node->next;
        list->head->prev = NULL;
    } else if (node == list->tail) {
        list->tail = node->prev;
        list->tail->next = NULL;
    } else {
        node->prev->next = node->next;
        node->next->prev = node->prev;
    }
    node->next = NULL;
    node->prev = NULL;
}

int find(DoublyLinkedList *list, const char *str) {
    int index = 0;
    Node *node = list->head;
    while (node) {
        if (strcmp(node->str, str) == 0)
            return index;
        index++;
        node = node->next;
    }
    return -1;
}

void print_list(DoublyLinkedList *list) {
    printf("[");
    Node *node = list->head;
    while (node) {
        printf("%s", node->str);
        if (node->next)
            printf(",");
        node = node->next;
    }
    printf("]\n");
}

int main(int argc, char **argv) {
    DoublyLinkedList list = {};
    print_list(&list);

    Node node_a = {.str="A"};
    Node node_b = {.str="BB"};
    Node node_c = {.str="CCC"};
    insert_end(&list, &node_a);
    insert_end(&list, &node_b);
    insert_end(&list, &node_c);
    print_list(&list);

    delete_node(&list, &node_b);
    delete_node(&list, &node_c);
    print_list(&list);

    insert_end(&list, &node_c);
    insert_end(&list, &node_b);
    print_list(&list);

    printf("%d\n", find(&list, "CCC"));
    printf("%d\n", find(&list, "Z"));

    return 0;
}