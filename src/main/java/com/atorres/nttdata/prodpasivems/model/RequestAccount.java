package com.atorres.nttdata.prodpasivems.model;

import com.atorres.nttdata.prodpasivems.utils.AccountCategory;
import com.atorres.nttdata.prodpasivems.utils.AccountType;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class RequestAccount {
    @JsonProperty("type")
    private AccountType type;
    private BigDecimal balance;
    @JsonProperty("category")
    private AccountCategory accountCategory;
}
