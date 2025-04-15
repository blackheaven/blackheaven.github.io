+++
title = "REST is nonsense"
date = 2025-04-15
draft = false
path = "2025-04/rest-nonsense"

[taxonomies]
categories = ["dev"]
tags = ["architecture", "engineering", "design"]
+++

Few months ago, I was digging into a bug we had: basically, from time to time,
we were marking orders as "paid", while Stripe [capture](https://docs.stripe.com/api/payment_intents/capture)
failed.

The payment was done is two steps: the user gives its card information to Stripe,
which captures the amount, and later on, when the order is validated, we call
this endpoint to definitively capture the funds.

I looked at logs, we still get 200 errors, _however_, the payload contains
a field `.status` which should be set to `"success"`, and obviously, it wasn't
checked.

The thing is, http responses come with a status code:

* 1xx informational response
* 2xx success
* 3xx redirection
* 4xx client errors
* 5xx server errors

The thing is, an error could not fit any of these, as they are business-related,
not protocol-related.

But, most of the implementations are mixing the two, so we assumed it was the
same thing too.

Let's focus on the requests for a moment.

Http comes with [methods/verbs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods),
they are all applied to a resource (the URI), the most common are:

* `GET` fetch a representation of the resource
* `POST` sends a payload to the resource
* `PUT` replace the resource
* `PATCH` replace some parts of the resource
* `DELETE` remove the resource

Moreover, REST [(which stands for Representational State Transfer)](https://en.wikipedia.org/wiki/REST)
adds constraints.

Let's try to draft few endpoints:

* Create an order
  + `POST /order`
* See an order
  + `GET /order/:orderId`
* Add an item to an order
  + `PUT /order/:orderId`
  + `PATCH /order/:orderId`
  + `POST /order/:orderId/item`
  + `POST /order/:orderId/item/:itemId`
* Cancel an order
  + `DELETE /order/:orderId`
* Request an order to be refunded
  + `POST /order/:orderId/refund`
  + `DELETE /order/:orderId`

REST add a structural constraint: everything should be a resource, bound by a
state.

Every "complex" interactions such as business-related processes, or (potential)
asynchronicity is, at best oddly handled.

Note: we could also mention UI/UX use cases, if you have a multi-users product,
a lot of the interactions or business-related processes depending on other
users' actions, which could happen at any time.

Alternatively, I would advocate to use things like [json-rpc](https://www.jsonrpc.org/).

Requests have 4 attributes:

* `jsonrpc`: a string with the protocol version
* `id`: an integer used to identify the response (requests can be batched)
* `method`: the method invoked
* `params`: an optional payload to perform the call

Response have 4 attributes:

* `jsonrpc`: a string with the protocol version
* `id`: an integer, matching the request
* `result`: an optional (on non-success) payload
* `error`: an optional (on success) payload

Let's rework our endpoints:

* Create an order
  + `method: "create-order"`
* See an order
  + `method: "fetch-order", params: {orderId: 42}`
* Add an item to an order
  + `method: "add-order-item", params: {orderId: 42, itemId: 1024}`
* Cancel an order
  + `method: "cancel-order", params: {orderId: 42}`
* Request an order to be refunded
  + `method: "request-refund-order", params: {orderId: 42}`

Here, we have gained a way to have a business-oriented API, focused on the
processes and actions, not on states.

On another hand, to avoid dealing with asynchronicity/synchronicity fork,
we could rely on [Server-sent events](https://en.wikipedia.org/wiki/Server-sent_events)
in a web context.

It's a cheap way to have a push-based architecture (instead of a pull-based
we have when we directly query the server).
