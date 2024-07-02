+++
title = "Pace in pair programming"
date = 2024-07-02
draft = false
path = "2024-07/pair-programming-pace"

[taxonomies]
categories = ["Code practice"]
tags = ["rust", "code kata", "coding dojo"]
+++

Few weeks ago I was [Software teamming/mob programming](https://en.wikipedia.org/wiki/Team_programming)
on the [Poker Hands kata](https://codingdojo.org/kata/PokerHands/).

I'm used to doing pair programming with entry/mid-level software developers in my
day-to-day job, one of the goal is knowledge sharing, and the other is to grow
their experience, to do so, I have few challenges:

* Influence, not command, to be sure that the task (e.g. the new feature, bug fix, etc.) will be eventually be done, by coming up with a plan (even though, I don't always state it) and re-orienting the code as needed
* Let enough space for them to experiment, but not too much, so we don't lose hours

But during this session, I was with another senior developer, so I have assumed
that we had both the same vision, and we could focus on moving forward on it
(instead of being slow down by programming languages or tooling details).

For reference, I was one of the drivers (the other one was not really active),
we were working with Java, but I'll re-do in Rust, trying to imitate what we did
to make my point.

We started by representing an invalid 2-cards hand, it should not be allowed at
some point, but we were in [Test-driven development/Test-first](https://en.wikipedia.org/wiki/Test-driven_development),
we needed to point of start to layout or types.

By default, when no other hand is found *high card* is picked as hand.

```rust
#[test]
fn two_unrelated_cards_should_be_high_card() {
    assert_eq!(
        Hand::new(&vec![Card::new(4, Suit::Club), Card::new(6, Suit::Diamond)]),
        Hand::HighCard
    );
}
```

Then, we decided to put some types, we have done few assumptions:

* `Value` will be correct (no need to do boundaries check, or represent Jack/Queen/Kind/Ace)
* We represent a `Vec<Card>` (`Collection<Card>` in Java), as it will eventually the correct number of `Card`s
* We avoid storing the *high card* value (Java does not have true [algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type), it would involve adding one more `class`)

Which gives us:

```rust
enum Suit {
    Club,
    Diamond,
    Heart,
    Spade,
}

struct Card {
    value: u8,
    suit: Suit,
}

impl Card {
    pub fn new(value: u8, suit: Suit) -> Self {
        Self { value, suit }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Hand {
    HighCard,
}

impl Hand {
    pub fn new(cards: &Vec<Card>) -> Self {
        return Hand::HighCard;
    }
}
```

It is simple enough to don't check the input.

Then we introduce *pair*, which happens when two cards have the same value:

```rust
#[test]
fn two_out_of_three_cards_have_the_same_value_should_be_pair() {
    assert_eq!(
        Hand::new(&vec![
            Card::new(5, Suit::Club),
            Card::new(6, Suit::Diamond),
            Card::new(5, Suit::Diamond)
        ]),
        Hand::Pair
    );
}
```

We can simply cheat for this one:

```rust
pub fn new(cards: &Vec<Card>) -> Self {
    if cards.len() == 3 {
        return Hand::Pair;
    }
    return Hand::HighCard;
}
```

Of course, it's not the definitive implementation, we can come up with another
test, so we can have a more probable one:

```rust
#[test]
fn three_unrelated_cards_should_be_high_card() {
    assert_eq!(
        Hand::new(&vec![
            Card::new(5, Suit::Club),
            Card::new(6, Suit::Diamond),
            Card::new(8, Suit::Club)
        ]),
        Hand::HighCard
    );
}
```

My colleague suggested to using a "simple" double loop:

```rust
pub fn new(cards: &Vec<Card>) -> Self {
    for i in 0..cards.len() - 1 {
        for j in (i + 1)..cards.len() {
            if cards[i].value == cards[j].value {
                return Hand::Pair;
            }
        }
    }
    return Hand::HighCard;
}
```

Note: for the record, writing this log forced me to rewrite the above code, and I had a off-by-error.

Then we wanted to have *three of a kind*, like pairs but with three cards having the same value:

```rust
#[test]
fn three_out_of_four_cards_have_the_same_value_should_be_three_of_a_kind() {
    assert_eq!(
        Hand::new(&vec![
            Card::new(5, Suit::Club),
            Card::new(6, Suit::Diamond),
            Card::new(5, Suit::Diamond),
            Card::new(5, Suit::Spade)
        ]),
        Hand::ThreeOfAKind
    );
}
```

The fix is simple: instead of directly returning when the value is identical,
we count it:

```rust
pub fn new(cards: &Vec<Card>) -> Self {
    for i in 0..cards.len() - 1 {
        let mut count = 1;
        for j in (i + 1)..cards.len() {
            if cards[i].value == cards[j].value {
                count += 1;
            }
        }
        if count == 3 {
            return Hand::ThreeOfAKind;
        }
        if count == 2 {
            return Hand::Pair;
        }
    }
    return Hand::HighCard;
}
```

Note: again, off-by-one error

Then we have *four of a kind*:

```rust
#[test]
fn four_out_of_five_cards_have_the_same_value_should_be_four_of_a_kind() {
    assert_eq!(
        Hand::new(&vec![
            Card::new(5, Suit::Club),
            Card::new(6, Suit::Diamond),
            Card::new(5, Suit::Diamond),
            Card::new(5, Suit::Heart),
            Card::new(5, Suit::Spade)
        ]),
        Hand::FourOfAKind
    );
}
```

Let's add one more `if`:

```rust
pub fn new(cards: &Vec<Card>) -> Self {
    for i in 0..cards.len() - 1 {
        let mut count = 1;
        for j in (i + 1)..cards.len() {
            if cards[i].value == cards[j].value {
                count += 1;
            }
        }
        if count == 4 {
            return Hand::FourOfAKind;
        }
        if count == 3 {
            return Hand::ThreeOfAKind;
        }
        if count == 2 {
            return Hand::Pair;
        }
    }
    return Hand::HighCard;
}
```

At this point, things became interesting with the *full house*, which is a
*pair* and a *three of a kind*:

```rust
#[test]
fn a_pair_and_a_three_of_a_kind_should_be_full_house() {
    assert_eq!(
        Hand::new(&vec![
            Card::new(5, Suit::Club),
            Card::new(6, Suit::Diamond),
            Card::new(5, Suit::Diamond),
            Card::new(5, Suit::Heart),
            Card::new(6, Suit::Spade)
        ]),
        Hand::FullHouse
    );
}
```

Here, I went wild, first, I suggested to accumulating counts in a `HashMap`, we all agreed on it:

```rust
let mut count_by_value = HashMap::new();
for card in cards.iter() {
    count_by_value
        .entry(card.value)
        .and_modify(|count| *count += 1)
        .or_insert(1);
}
```

Note: Finally, we got rid of the nested loop

Note 2: no off-by-one error this time

Then I proposed to iterate again on the counts so we have a definitive map count.

At this point, I have felt others doubting, or at least, having trouble to know where we were heading.

I have changed my idea to use `contains` on counts:

```rust
pub fn new(cards: &Vec<Card>) -> Self {
    let mut count_by_value = HashMap::new();
    for card in cards.iter() {
        count_by_value
            .entry(card.value)
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }

    let count_by_value: Vec<usize> = count_by_value.into_values().collect();
    if count_by_value.contains(&3) && count_by_value.contains(&2) {
        return Hand::FullHouse;
    }
    if count_by_value.contains(&4) {
        return Hand::FourOfAKind;
    }
    if count_by_value.contains(&3) {
        return Hand::ThreeOfAKind;
    }
    if count_by_value.contains(&2) {
        return Hand::Pair;
    }

    return Hand::HighCard;
}
```

They were surprised (that it worked, and that it was so simple).

I think, at this point, I was thinking to the next step, and having this
misunderstanding gave me some room to take a step back and resynchronize with
the pace of the group.

Then we have two pairs:

```rust
#[test]
fn two_pairs_out_of_five_cards_kind_should_be_two_pairs() {
    assert_eq!(
        Hand::new(&vec![
            Card::new(5, Suit::Club),
            Card::new(6, Suit::Diamond),
            Card::new(5, Suit::Diamond),
            Card::new(7, Suit::Heart),
            Card::new(6, Suit::Spade)
        ]),
        Hand::TwoPairs
    );
}
```

Here came my previous strategy, but this time, it was not over-engineered, and the group was ready:

```rust
pub fn new(cards: &Vec<Card>) -> Self {
    let mut count_by_value = HashMap::new();
    for card in cards.iter() {
        count_by_value
            .entry(card.value)
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }

    let mut count_by_value_count = HashMap::new();
    for count in count_by_value.into_values() {
        count_by_value_count
            .entry(count)
            .and_modify(|count_count| *count_count += 1)
            .or_insert(1);
    }

    if count_by_value_count.contains_key(&3) && count_by_value_count.contains_key(&2) {
        return Hand::FullHouse;
    }
    if count_by_value_count.contains_key(&4) {
        return Hand::FourOfAKind;
    }
    if count_by_value_count.contains_key(&3) {
        return Hand::ThreeOfAKind;
    }
    if count_by_value_count.get(&2) == Some(&2) {
        return Hand::TwoPairs;
    }
    if count_by_value_count.contains_key(&2) {
        return Hand::Pair;
    }

    return Hand::HighCard;
}
```

At this point, the implementation became a bit big, let's extract our counting function:

```rust
impl Hand {
    pub fn new(cards: &Vec<Card>) -> Self {
        let count_by_value = Self::count_by_key(cards.iter(), |card| card.value);
        let count_by_value_count = Self::count_by_key(count_by_value.into_values(), |count| count);

        if count_by_value_count.contains_key(&3) && count_by_value_count.contains_key(&2) {
            return Hand::FullHouse;
        }
        if count_by_value_count.contains_key(&4) {
            return Hand::FourOfAKind;
        }
        if count_by_value_count.contains_key(&3) {
            return Hand::ThreeOfAKind;
        }
        if count_by_value_count.get(&2) == Some(&2) {
            return Hand::TwoPairs;
        }
        if count_by_value_count.contains_key(&2) {
            return Hand::Pair;
        }

        return Hand::HighCard;
    }

    fn count_by_key<K, V, Values, F>(values: Values, to_key: F) -> HashMap<K, usize>
    where
        K: core::hash::Hash + std::cmp::Eq,
        Values: Iterator<Item = V>,
        F: Fn(V) -> K,
    {
        let mut count_by_value: HashMap<K, usize> = HashMap::new();
        for value in values {
            let k: K = to_key(value);
            count_by_value
                .entry(k)
                .and_modify(|count| *count += 1)
                .or_insert(1);
        }
        return count_by_value;
    }
}
```

we can extract it further as it will be call twice anyway:

```rust
impl Hand {
    pub fn new(cards: &Vec<Card>) -> Self {
        let count_of_count_by_value = Self::count_of_count_by_key(cards.iter(), |card| card.value);

        if count_of_count_by_value.contains_key(&3) && count_of_count_by_value.contains_key(&2) {
            return Hand::FullHouse;
        }
        if count_of_count_by_value.contains_key(&4) {
            return Hand::FourOfAKind;
        }
        if count_of_count_by_value.contains_key(&3) {
            return Hand::ThreeOfAKind;
        }
        if count_of_count_by_value.get(&2) == Some(&2) {
            return Hand::TwoPairs;
        }
        if count_of_count_by_value.contains_key(&2) {
            return Hand::Pair;
        }

        return Hand::HighCard;
    }

    fn count_of_count_by_key<K, V, Values, F>(values: Values, to_key: F) -> HashMap<usize, usize>
    where
        K: core::hash::Hash + std::cmp::Eq,
        Values: Iterator<Item = V>,
        F: Fn(V) -> K,
    {
        let count_by_value = Self::count_by_key(values, to_key);
        return Self::count_by_key(count_by_value.into_values(), |count| count);
    }

    // ...
}
```

Until know, we have ignored `Suit`, let's take *flush*, which expects the
five cards to be in the same `Suit`:

```rust
#[test]
fn five_cards_with_the_same_suit_should_be_flush() {
    assert_eq!(
        Hand::new(&vec![
            Card::new(5, Suit::Diamond),
            Card::new(6, Suit::Diamond),
            Card::new(2, Suit::Diamond),
            Card::new(7, Suit::Diamond),
            Card::new(9, Suit::Diamond)
        ]),
        Hand::Flush
    );
}
```

We can leverage our `count_of_count_by_key` to make this test pass:

```rust
pub fn new(cards: &Vec<Card>) -> Self {
    let count_of_count_by_suit = Self::count_of_count_by_key(cards.iter(), |card| card.suit);
    if count_of_count_by_suit.contains_key(&5) {
        return Hand::Flush;
    }

    let count_of_count_by_value = Self::count_of_count_by_key(cards.iter(), |card| card.value);
    if count_of_count_by_value.contains_key(&3) && count_of_count_by_value.contains_key(&2) {
        return Hand::FullHouse;
    }
    if count_of_count_by_value.contains_key(&4) {
        return Hand::FourOfAKind;
    }
    if count_of_count_by_value.contains_key(&3) {
        return Hand::ThreeOfAKind;
    }
    if count_of_count_by_value.get(&2) == Some(&2) {
        return Hand::TwoPairs;
    }
    if count_of_count_by_value.contains_key(&2) {
        return Hand::Pair;
    }

    return Hand::HighCard;
}
```

So far, so good, it remains two cases, *straight* happens when the five cards:

```rust
#[test]
fn five_1_increasing_cards_with_should_be_straight() {
    assert_eq!(
        Hand::new(&vec![
            Card::new(5, Suit::Spade),
            Card::new(6, Suit::Diamond),
            Card::new(2, Suit::Diamond),
            Card::new(4, Suit::Diamond),
            Card::new(3, Suit::Diamond)
        ]),
        Hand::Straight
    );
}
```

My colleague suggested to sort cards by values and check from card to cards if
they have and increment of one:

```rust
let mut is_straight = true;
// Rust prevents us to sort in-place an immutable vector
let mut cards_value: Vec<u8> = cards.iter().map(|card| card.value).collect();
cards_value.sort();
for i in 1..cards_value.len() {
    if cards_value[i - 1] + 1 != cards_value[i] {
        is_straight = false;
    }
}

if is_straight {
    return Hand::Straight;
}
```

I'm actually not a fan of mutation, let's refactor it with streams:

```rust
let mut cards_value: Vec<u8> = cards.iter().map(|card| card.value).collect();
cards_value.sort();
let is_straight = cards_value
    .iter()
    .zip(cards_value.iter().skip(1))
    .all(|(&x, &y)| x + 1 == y);

if is_straight {
    return Hand::Straight;
}
```

Finally, we have the *straight flush*, all cards form a sequence 1-increasing
values, with the same suit:

```rust
#[test]
fn five_1_increasing_cards_with_same_suit_with_should_be_straight_flush() {
    assert_eq!(
        Hand::new(&vec![
            Card::new(5, Suit::Diamond),
            Card::new(6, Suit::Diamond),
            Card::new(2, Suit::Diamond),
            Card::new(4, Suit::Diamond),
            Card::new(3, Suit::Diamond)
        ]),
        Hand::StraightFlush
    );
}
```

We mostly have to switch lines, add a condition, and here is our final result:

```rust
pub fn new(cards: &Vec<Card>) -> Self {
    let mut cards_value: Vec<u8> = cards.iter().map(|card| card.value).collect();
    cards_value.sort();
    let is_straight = cards_value
        .iter()
        .zip(cards_value.iter().skip(1))
        .all(|(&x, &y)| x + 1 == y);
    let is_flush = Self::count_of_count_by_key(cards.iter(), |card| card.suit).contains_key(&5);

    if is_flush && is_straight {
        return Hand::StraightFlush;
    }

    if is_straight {
        return Hand::Straight;
    }

    if is_flush {
        return Hand::Flush;
    }

    let count_of_count_by_value = Self::count_of_count_by_key(cards.iter(), |card| card.value);
    if count_of_count_by_value.contains_key(&3) && count_of_count_by_value.contains_key(&2) {
        return Hand::FullHouse;
    }
    if count_of_count_by_value.contains_key(&4) {
        return Hand::FourOfAKind;
    }
    if count_of_count_by_value.contains_key(&3) {
        return Hand::ThreeOfAKind;
    }
    if count_of_count_by_value.get(&2) == Some(&2) {
        return Hand::TwoPairs;
    }
    if count_of_count_by_value.contains_key(&2) {
        return Hand::Pair;
    }

    return Hand::HighCard;
}
```

I really enjoyed this session, but, beyond that, I have two points to highlight:

* Pace is important, whatever the group you are in
* Picking the next test/requirement is the other cornerstone to have a regular pace
