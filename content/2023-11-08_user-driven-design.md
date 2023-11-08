+++
title = "User Driven Design"
date = 2022-11-08
draft = false
path = "2023-11/user-driven-design"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "product", "design"]
+++

Last week I was watching [Mauro Servienti - All Our Aggregates Are Wrong](https://www.youtube.com/watch?v=KkzvQSuYd5I).

At some point he stated that users tend to think in terms of data.

I had a female friend, a long time ago, which was studying to be part of
private clinics/public hospitals management team.
Let  me emphasize this, her future position would include: people management,
hiring, fundraising, budget management.

Despite that, she had some lessons on "computer science", I had a look at some of her homework, none of them were making sense.
Basically, on of them was a kind of database table modeling (mostly boxes with a name, attributes, and links between them).

Once the initial shock passed:

* I'll be in trouble if I have to make a project with them
* We (our industry) have done really, really, bad if our stakeholder has to be trained to speak with us

To illustrate that, let's say you discuss with someone in charge of an
e-commerce website, (s)he might describe a basket with:

* Items
* Discounts
* Price
* Shipping address
* Carrier
* Status

So, you'll be tempted to model it as:

```haskell
data Basket = Basket
  { items :: [Item]
  , discounts :: [Discount]
  , totalPrice :: Price
  , shippingAddress :: Maybe Address
  , carrier :: Maybe Carrier
  , status :: Status
  }
```

Great, you have just let the stakeholder strap you in his/her design.

Just to be sure we are on the same page: when you go to the surgeon, would you
request him/her what you want (i.e. fix your broken arm), or how you want it
(i.e. the complete operating protocol).

Note: IMO, focusing on the *how* denotes a lack of trust

Whenever I tackle a subject, I start with the actions, let's grab our
stakeholder again, a customer will:

* Fill the basket
* Select the carrier
* Pay
* Being delivered

To give a hint on what goes wrong, the carrier does not care about the discounts.
Everything is coupled around `Basket` which is also an issue since we cannot
express that a shipping address have been set before going in the shipping part.

That's when I leverage [DDD's Bounded Contexts](https://martinfowler.com/bliki/BoundedContext.html).

I can then come up with:

```haskell
computePackage :: [Items] -> Package -- size, weight, insured price
carriersProposal :: Package -> Address -> m [CarrierProposal] -- Name, Price, Delay/Proposed delivery options
bill :: [Item] -> [ExtraDiscount] -> CarrierProposal -> Bill
pay :: Bill -> m ()
```

Then, and only then, I can work on my types structure, without being restrained
by my stakeholder design.
