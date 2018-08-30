from fairness.metrics.FilterGroup import FilterGroup
from fairness.metrics.Metric import Metric


class SubgroupMetric(Metric):
    """
    Takes the given metric and creates a version of it that calculates
    metrics per subgroup.
    It measures each protected subgroup against the unprotected group.

    For example, for SubgroupMetric(CV), this measure calculates the CV score
    for each comparison between the unprotected group and any protected subgroup,
    and returns the maximum value as the final discrimination score.
    """
    def __init__(self, metric_class):
        Metric.__init__(self)
        self.metric = metric_class
        self.name = self.metric().get_name()   # to be modified as this metric is expanded

    def calc(self, actual, predicted, dict_of_sensitive_lists, single_sensitive_name,
             unprotected_vals, positive_pred,
             algorithm=None, dataset=None, tag=None):
        "modified to calculate metric first for each subgroup and then take max"
        sgfilter = FilterGroup(self.metric())
        sgfilter.set_subgroup_sensitive(self.sensitive_attr)
        return sgfilter.calc(actual, predicted, dict_of_sensitive_lists, single_sensitive_name,
                             unprotected_vals, positive_pred,
                             algorithm, dataset, tag)

    def expand_per_dataset(self, dataset, sensitive_dict, tag):
        objects_list = []
        for sensitive in dataset.get_sensitive_attributes_with_joint():
            objects_list += self.make_subgroup_obj(sensitive)
        return objects_list

    def make_subgroup_obj(self, sensitive_attr):
        obj = self.__class__(self.metric)
        obj.set_subgroup_sensitive(sensitive_attr)
        return obj

    def set_subgroup_sensitive(self, sensitive_name):
        """
        Set the attribute and value to filter, i.e., to calculate this metric for.
        """
        self.sensitive_attr = sensitive_name
        self.name = "Subgroups: " + str(sensitive_name) + "-" + self.name
