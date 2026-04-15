#include <bits/stdc++.h>
using namespace std;

class SkipList {
private:

    /* 
        Node structure:
        Each node stores:
        - value
        - forward pointers (one for each level)
    */
    struct Node {
        int value;
        vector<Node*> forward;

        // Constructor
        Node(int val, int level) {
            value = val;
            forward.resize(level + 1, nullptr);
        }
    };

    int MAX_LEVEL;     // Maximum level allowed in skiplist
    float P;           // Probability for random level generation
    int level;         // Current highest level in skiplist
    Node* header;      // Header node (start of skiplist)

    /*
        Generate random level for new node
        With probability P, increase level
    */
    int randomLevel() {
        int lvl = 0;

        // Keep increasing level with probability P
        while (((float)rand() / RAND_MAX) < P && lvl < MAX_LEVEL)
            lvl++;

        return lvl;
    }

public:

    /*
        Constructor
        Initialize skiplist with:
        - max level
        - probability
    */
    SkipList(int maxLevel = 16, float prob = 0.5) {
        MAX_LEVEL = maxLevel;
        P = prob;
        level = 0;

        // Create header node with max level
        header = new Node(-1, MAX_LEVEL);
    }


    /*
        SEARCH operation
        Start from top level and move down
    */
    bool search(int target) {

        Node* curr = header;

        // Traverse from highest level to lowest
        for (int i = level; i >= 0; i--) {

            // Move forward while value is smaller
            while (curr->forward[i] != nullptr &&
                   curr->forward[i]->value < target) {

                curr = curr->forward[i];
            }
        }

        // Move to next node at level 0
        curr = curr->forward[0];

        // Check if found
        if (curr != nullptr && curr->value == target)
            return true;

        return false;
    }


    /*
        INSERT operation
    */
    void insert(int value) {

        // Stores nodes that need pointer updates
        vector<Node*> update(MAX_LEVEL + 1);

        Node* curr = header;

        // Find positions where node should be inserted
        for (int i = level; i >= 0; i--) {

            while (curr->forward[i] != nullptr &&
                   curr->forward[i]->value < value) {

                curr = curr->forward[i];
            }

            update[i] = curr;
        }

        // Move to level 0
        curr = curr->forward[0];

        // Insert only if value not already present
        if (curr == nullptr || curr->value != value) {

            // Generate random level
            int newLevel = randomLevel();

            // If new level higher, initialize header pointers
            if (newLevel > level) {
                for (int i = level + 1; i <= newLevel; i++)
                    update[i] = header;

                level = newLevel;
            }

            // Create new node
            Node* newNode = new Node(value, newLevel);

            // Update forward pointers
            for (int i = 0; i <= newLevel; i++) {

                newNode->forward[i] = update[i]->forward[i];
                update[i]->forward[i] = newNode;
            }
        }
    }


    /*
        DELETE operation
    */
    void erase(int value) {

        vector<Node*> update(MAX_LEVEL + 1);
        Node* curr = header;

        // Find node to delete
        for (int i = level; i >= 0; i--) {

            while (curr->forward[i] != nullptr &&
                   curr->forward[i]->value < value) {

                curr = curr->forward[i];
            }

            update[i] = curr;
        }

        curr = curr->forward[0];

        // If node found
        if (curr != nullptr && curr->value == value) {

            // Update forward pointers
            for (int i = 0; i <= level; i++) {

                // If pointer doesn't match, stop
                if (update[i]->forward[i] != curr)
                    break;

                update[i]->forward[i] = curr->forward[i];
            }

            // Delete node
            delete curr;

            // Adjust level if needed
            while (level > 0 &&
                   header->forward[level] == nullptr) {

                level--;
            }
        }
    }


    /*
        Print skiplist (for debugging)
    */
    void print() {
        cout << "\nSkip List:\n";

        for (int i = level; i >= 0; i--) {

            Node* node = header->forward[i];

            cout << "Level " << i << ": ";

            while (node != nullptr) {
                cout << node->value << " ";
                node = node->forward[i];
            }

            cout << endl;
        }
    }
};



/*
    Driver Code
*/
int main() {

    SkipList sl;

    // Insert elements
    sl.insert(3);
    sl.insert(6);
    sl.insert(7);
    sl.insert(9);
    sl.insert(12);
    sl.insert(19);

    sl.print();

    // Search
    cout << "\nSearch 6: " << sl.search(6) << endl;
    cout << "Search 15: " << sl.search(15) << endl;

    // Delete
    sl.erase(6);

    cout << "\nAfter deleting 6:";
    sl.print();

    return 0;
}