Array.prototype.last = function () {
  return this[this.length - 1];
};

Array.prototype.shallowCopy = function () {
  return this.slice(0);
};
