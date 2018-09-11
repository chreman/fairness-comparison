from fairness.metrics.Metric import Metric
import pandas as pd

class FilterGroup(Metric):
    def __init__(self, metric):
        Metric.__init__(self)
        self.metric = metric
        self.name = metric.get_name()

    def calc(self, actual, predicted, dict_of_sensitive_lists, single_sensitive_name,
             unprotected_vals, positive_pred,
             algorithm=None, dataset=None, tag=None):

        # print("unprotected_vals", unprotected_vals)
        # print("sensitive_for_metric", self.sensitive_for_metric)
        sensitive = dict_of_sensitive_lists[self.sensitive_for_metric]
        sg_metrics = []
        # print("protected_vals", set(sensitive))
        dfs = []
        for sens_expr in set(sensitive):
            # print(sens_expr)
            # make for loop here - for each sens_val do calc
            # return max
            # print("actual, sensitive")
            # print(len(actual), len(sensitive))
            # print(set(actual), set(sensitive))
            sg_actual = [act for act, sens in zip(actual, sensitive)
                         if sens in [sens_expr] + unprotected_vals]
            # print("actual_sens")
            # print(len(sg_actual))
            # print(set(sg_actual))
            sg_predicted = [pred for pred, sens in zip(predicted, sensitive)
                            if sens in [sens_expr] + unprotected_vals]
            # print("predicted_sens")
            # print(len(sg_predicted))
            # print(set(sg_predicted))
            sg_sensitive = [sens for sens in sensitive
                            if sens == sens_expr]
            # print(len(sg_sensitive) / len(actual))

            filtered_dict = {}
            for sens_val in dict_of_sensitive_lists:
                # print(sens_val)
                other_sensitive = dict_of_sensitive_lists[sens_val]
                # print("other sensitive")
                # print(len(other_sensitive))
                # print(set(other_sensitive))
                filtered = [s for s, sens in zip(other_sensitive, sensitive)
                            if sens in [sens_expr] + unprotected_vals]
                # print("filtered")
                # print(len(filtered))
                # print(set(filtered))
                filtered_dict[sens_val] = filtered
            sg_metric = self.metric.calc(sg_actual, sg_predicted,
                                         filtered_dict,
                                         single_sensitive_name,
                                         unprotected_vals, positive_pred)
            sg_metrics.append(sg_metric)
            dfs.append(pd.DataFrame.from_dict({
                                     "dataset": dataset,
                                     "algorithm": algorithm,
                                     "metric_name": self.name,
                                     "tag": tag,
                                     "subgroup": sens_expr,
                                     "metric": sg_metric}, orient='index').T)
        df = pd.concat(dfs)
        df.to_csv("results/sg_metrics/" + "_".join(["sg_metric", "results"])+".csv",
                  mode="a", header=False)
        return max(sg_metrics)

    def set_subgroup_sensitive(self, sensitive_name):
        """
        Sets the sensitive attr to compare.

        sensitive_name        sensitive attribute name (e.g., 'race')
        """
        self.name += str(sensitive_name)
        self.sensitive_for_metric = sensitive_name
