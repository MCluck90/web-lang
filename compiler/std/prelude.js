Array.prototype.last = function () {
  return this[this.length - 1];
};

Array.prototype.shallow$copy = function () {
  return this.slice(0);
};
